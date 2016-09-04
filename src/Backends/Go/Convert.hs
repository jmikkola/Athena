module Backends.Go.Convert where

import qualified Backends.Go.Syntax as Syntax
import qualified AST.Expression as E
import qualified Type as T
import IR

type Result = Either String

convertDecl :: IR.Decl -> Result Syntax.Declaration
convertDecl decl = case decl of
  IR.StmtDecl stmt
    -> case stmt of
        IR.Let name typ expr
          -> case expr of
              IR.Lambda t args stmt
                -> do
                  decl <- makeFnDecl t args
                  body <- convertStmt stmt
                  return $ Syntax.Function name decl body
              _
                -> do
                  typ' <- convertType typ
                  expr' <- convertExpr expr
                  return $ Syntax.Variable name (Just typ') expr'
        _
          -> fail $ "cannot convert `" ++ show stmt ++ "` to a module-level declaration"
  IR.TypeDecl name typ
    -> do
      typ' <- convertType typ
      undefined -- TODO: enums can convert to multiple declarations

makeFnDecl typ argNames = undefined

convertStmt :: IR.Statement -> Result Syntax.Statement
convertStmt _ = error "TODO convertStmt"

convertValue :: IR.Value -> Result Syntax.Expression
convertValue val = case val of
  IR.StrVal s
    -> return $ Syntax.StrVal s
  IR.BoolVal b
    -> return $ Syntax.BoolVal b
  IR.IntVal i
    -> return $ Syntax.IntVal i
  IR.FloatVal f
    -> return $ Syntax.FloatVal f
  IR.StructVal n fs
    -> do
      fields <- mapMSnd convertExpr fs
      return $ Syntax.StructVal n fields

convertExpr :: IR.Expression -> Result Syntax.Expression
convertExpr expr = case expr of
  IR.Paren e _
    -> do
      e' <- convertExpr e
      return $ Syntax.Paren e'
  IR.Val val
    -> convertValue val
  IR.Unary _ op e
    -> do
      e' <- convertExpr e
      op' <- converUnaryOp op
      return $ Syntax.Unary op' e'
  IR.Binary _ op l r
    -> do
      l' <- convertExpr l
      r' <- convertExpr r
      case op of
       E.Power
         -- TODO: Handle taking powers of ints
         -> let powFN = Syntax.FieldAccess (Syntax.Var "math") "Pow"
            in return $ Syntax.Call powFN [l', r']
       _
         -> do
           op' <- convertBinaryOp op
           return $ Syntax.Binary op' l' r'
  IR.Call _ f args
    -> do
      fEx <- convertExpr f
      argEx <- mapM convertExpr args
      return $ Syntax.Call fEx argEx
  IR.Cast t ex
    -> do
      expr' <- convertExpr ex
      typ <- convertType t
      return $ Syntax.TypeCast typ expr'
  IR.Var _ n
    -> return $ Syntax.Var n
  IR.Access _ e n
    -> do
      e' <- convertExpr e
      return $ Syntax.FieldAccess e' n
  IR.Lambda _ _ _
    -> undefined -- TODO

convertType :: T.Type -> Result Syntax.Type
convertType t = case t of
  T.String
    -> return Syntax.GoString
  T.Float
    -> return Syntax.GoFloat64
  T.Int
    -> return Syntax.GoInt64
  T.Bool
    -> return Syntax.GoBool
  T.Nil
    -> return Syntax.GoVoid -- TODO: come up with a better approach
  T.Function ts t'
    -> do
      argTypes <- mapM convertType ts
      retType <- convertType t'
      let r = case retType of
            Syntax.GoVoid -> []
            _             -> [retType]
      return $ Syntax.GoFunc argTypes r
  T.TypeName name -- TODO: this should either be GoStruct or GoInterface
    -> return $ Syntax.TypeName name
  T.Struct fields
    -> do
      fields' <- mapMSnd convertType fields
      undefined -- TODO
  T.Enum  _
    -> undefined -- some interface, but which?

converUnaryOp :: E.UnaryOp -> Result Syntax.UnaryOp
converUnaryOp E.BitInvert
  = return Syntax.BitInvert
converUnaryOp E.BoolNot
  = return Syntax.BoolNot

convertBinaryOp :: E.BinOp -> Result Syntax.BinaryOp
convertBinaryOp op = case op of
  E.Plus
    -> return Syntax.Plus
  E.Minus
    -> return Syntax.Minus
  E.Times
    -> return Syntax.Times
  E.Divide
    -> return Syntax.Divide
  E.Mod
    -> return Syntax.Mod
  E.BitAnd
    -> return Syntax.BitAnd
  E.BitOr
    -> return Syntax.BitOr
  E.BitXor
    -> return Syntax.BitXor
  E.BoolAnd
    -> return Syntax.BoolAnd
  E.BoolOr
    -> return Syntax.BoolOr
  E.Eq
    -> return Syntax.Eq
  E.NotEq
    -> return Syntax.NotEq
  E.Less
    -> return Syntax.Less
  E.LessEq
    -> return Syntax.LessEq
  E.Greater
    -> return Syntax.Greater
  E.GreaterEq
    -> return Syntax.GreaterEq
  E.LShift
    -> return Syntax.LShift
  E.RShift
    -> return Syntax.RShift
  E.RRShift
    -> return Syntax.RRShift
  _
    -> fail $ show op ++ " should have been already handled"

mapMSnd :: (Monad m) => (a -> m b) -> [(c, a)] -> m [(c, b)]
mapMSnd f = mapM f'
  where f' (c, a) = do
          b <- f a
          return (c, b)
