module Backends.Go.Convert where

import qualified Backends.Go.Syntax as Syntax
import qualified AST.Expression as E
import IR

type Result = Either String

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
         -> undefined -- TODO
       E.BitXor
         -> undefined -- TODO
       _
         -> do
           op' <- convertBinaryOp op
           return $ Syntax.Binary op' l' r'
  IR.Call _ f args
    -> do
      fEx <- convertExpr f
      argEx <- mapM convertExpr args
      return $ Syntax.Call fEx argEx
  IR.Cast _ _
    -> undefined -- TODO
  IR.Var _ n
    -> return $ Syntax.Var n
  IR.Access _ e n
    -> do
      e' <- convertExpr e
      return $ Syntax.FieldAccess e' n
  IR.Lambda _ _ _
    -> undefined -- TODO

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
