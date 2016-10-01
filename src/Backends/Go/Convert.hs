module Backends.Go.Convert where

import Control.Monad (liftM)

import qualified Backends.Go.Syntax as Syntax
import qualified AST.Expression as E
import qualified Type as T
import IR

type Result = Either String

convertFile :: [IR.Decl] -> Result [Syntax.Declaration]
convertFile file = liftM concat $ mapM convertDecl file

convertDecl :: IR.Decl -> Result [Syntax.Declaration]
convertDecl decl = case decl of
  IR.StmtDecl stmt -> case stmt of
    IR.Let name typ expr -> case expr of
      IR.Lambda t args stmt' -> do
        decl' <- makeFnDecl t args
        body <- convertBlock stmt'
        return [Syntax.Function name decl' body]
      _ -> do
        typ' <- convertType typ
        expr' <- convertExpr expr
        return [Syntax.Variable name (Just typ') expr']
    _ ->
      fail $ "cannot convert `" ++ show stmt ++ "` to a module-level declaration"
  IR.TypeDecl name typ -> case typ of
    T.Struct _ fields -> do
      fields' <- mapMSnd convertType fields
      stringMethod <- makeStringMethod name fields'
      return [Syntax.Structure name fields', stringMethod]
    T.Enum _ options -> do
      structures <- makeStructures name options
      let iface = Syntax.Interface name [(enumMethodName name, enumTagMethodDecl)]
      return $ iface : structures
    _ ->
      fail $ "cannot emit declaration for " ++ show typ

makeStringMethod :: String -> [(String, Syntax.Type)] -> Result Syntax.Declaration
makeStringMethod structName fields =
  let fieldNames = map fst fields
      decl = Syntax.FunctionDecl [] [Syntax.JustType $ Syntax.GoString]
      sprintf = Syntax.FieldAccess (Syntax.Var "fmt") "Sprintf"
      format = Syntax.StrVal $ concatMap (++ ": %v,\n") fieldNames
      values = map (Syntax.FieldAccess (Syntax.Var "self")) fieldNames
      printExpr = Syntax.Call sprintf (format : values)
      printStmt = Syntax.Return printExpr
      body = Syntax.Block [printStmt]
  in return $ Syntax.Method ("self", Syntax.GoStruct structName) "String" decl body

enumTagMethodDecl :: Syntax.FunctionDecl
enumTagMethodDecl = Syntax.FunctionDecl [] []

enumMethodName :: String -> String
enumMethodName = (++) "Enum"

makeStructures :: String -> [(String, [(String, T.Type)])] -> Result [Syntax.Declaration]
makeStructures name options = (liftM concat) (mapM (makeStructure name) options)

makeStructure :: String -> (String, [(String, T.Type)]) -> Result [Syntax.Declaration]
makeStructure name (optName, fields) = do
  fields' <- mapMSnd convertType fields
  strMethod <- makeStringMethod name fields'
  let structDef = Syntax.Structure optName fields'
  let recvType = Syntax.TypeName name
  let methodDef = Syntax.Method ("self", recvType) (enumMethodName optName)
                  enumTagMethodDecl (Syntax.Block [])
  return [structDef, methodDef, strMethod]

makeFnDecl :: T.Type -> [String] -> Result Syntax.FunctionDecl
makeFnDecl (T.Function ats rts) argNames = do
  argTs <- mapM convertType ats
  retT <- convertType rts
  let args = map (\(t,n) -> Syntax.NamedType n t) (zip argTs argNames)
  return $ Syntax.FunctionDecl args [Syntax.JustType retT]
makeFnDecl t _ = fail $ "cannot use type " ++ show t ++ " for function decl"

convertBlock :: IR.Statement -> Result Syntax.Statement
convertBlock stmt = do
  stmts' <- case stmt of
    IR.Block _ stmts -> concatMapM convertStmt stmts
    _                -> convertStmt stmt
  return $ Syntax.Block stmts'

convertStmt :: IR.Statement -> Result [Syntax.Statement]
convertStmt stmt = case stmt of
  IR.Return Nothing ->
    return [Syntax.JustReturn]
  IR.Return (Just e) -> do
    e' <- convertExpr e
    return [Syntax.Return e']
  IR.Let s t e -> do
    e' <- convertExpr e
    t' <- convertType t
    return [ Syntax.VarStmt s (Just t') e',
             Syntax.VarStmt "_" (Just t') (Syntax.Var s)]
  IR.Assign fs e -> do
    e' <- convertExpr e
    return [Syntax.Assign fs e']
  IR.Block _ _ -> do
    blk <- convertBlock stmt
    return [blk]
  IR.Expr e -> do
    e' <- convertExpr e
    return [Syntax.Expr e']
  IR.If e st melse -> do
    e' <- convertExpr e
    st' <- convertBlock st
    melse' <- mapM convertBlock melse
    return [Syntax.If e' st' melse']
  IR.While e st -> do
    e' <- convertExpr e
    st' <- convertBlock st
    return [Syntax.For1 e' st']

convertValue :: IR.Value -> Result Syntax.Expression
convertValue val = case val of
  IR.StrVal s       -> return $ Syntax.StrVal s
  IR.BoolVal b      -> return $ Syntax.BoolVal b
  IR.IntVal i       -> return $ Syntax.IntVal i
  IR.FloatVal f     -> return $ Syntax.FloatVal f
  IR.StructVal t fs -> do
    let name = T.nameOf t
    fields <- mapMSnd convertExpr fs
    return $ Syntax.Reference $ Syntax.StructVal name fields

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
  IR.Var _ n ->
    if n == "print"
    then return $ Syntax.FieldAccess (Syntax.Var "fmt") "Println"
    else return $ Syntax.Var n
  IR.Access _ e n -> do
    e' <- convertExpr e
    return $ Syntax.FieldAccess e' n
  IR.Lambda t argNames body -> do
    t' <- convertType t
    body' <- convertBlock body
    return $ Syntax.Func t' argNames body'

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

  T.Struct n _ ->
    return $ Syntax.GoStruct n
  T.Enum n _ ->
    return $ Syntax.GoInterface n

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

concatMapM :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f list = (liftM concat) (mapM f list)
