module Eval where

import Data.Bits (complement)
import Data.Map (Map)
import qualified Data.Map as Map

import Parse

data Expr = IntValExpr Int
          | FloatValExpr Float
          | StringValExpr String
          | StructValExpr String
          | VarExpr String
          | UnaryExpr UnaryOp Expr
          | BinaryExpr BinaryOp Expr Expr
          deriving (Eq, Show)

data EvalError = EvalError { message :: String
                           , expr :: Expr
                           }
                 deriving (Eq, Show)

type EvalContext = Map String Expr

translate :: Expression -> Expr
translate (ExpressionLit lit)   = case lit of
  LiteralInt i    -> IntValExpr i
  LiteralFloat f  -> FloatValExpr f
  LiteralString s -> StringValExpr s
  LiteralStruct s -> StructValExpr s
translate (ExpressionVar name)      = VarExpr name
translate (ExpressionParen inner)   = translate inner
translate (ExpressionFnCall _ _)    = error "TODO"
translate (ExpressionBinary op l r) = BinaryExpr op (translate l) (translate r)
translate (ExpressionUnary op ex)   = UnaryExpr op (translate ex)

evalExpr :: EvalContext -> Expr -> Either EvalError Expr
evalExpr ctx ex =
  case ex of
    (IntValExpr _)      -> Right ex
    (FloatValExpr _)    -> Right ex
    (StringValExpr _)   -> Right ex
    (StructValExpr _)   -> Right ex
    (VarExpr varname)   ->
      case Map.lookup varname ctx of
        Just value -> Right value
        Nothing    -> Left $ EvalError { message="Variable not found", expr=ex }
    (UnaryExpr op ex')   -> do
      inner <- evalExpr ctx ex'
      wrapError ex $ evalUnary op inner
    (BinaryExpr op l r) -> do
      left <- evalExpr ctx l
      right <- evalExpr ctx r
      wrapError ex $ evalBinary op left right

wrapError :: Expr -> Either String Expr -> Either EvalError Expr
wrapError original result = case result of
  Right r -> Right r
  Left er -> Left $ EvalError { message=er, expr=original }

evalUnary :: UnaryOp -> Expr -> Either String Expr
evalUnary op ex =
  case ex of
    (IntValExpr i)    ->
      case op of
        Negate -> Right . IntValExpr $ 0 - i
        Flip   -> Right . IntValExpr $ complement i
    (FloatValExpr f)  ->
      case op of
        Negate -> Right . FloatValExpr $ 0 - f
        _      -> Left $ "Can't apply unary op " ++ (display op) ++ " to a float"
    _                 -> Left $ "Can't apply unary op " ++ (display op)

evalBinary :: BinaryOp -> Expr -> Expr -> Either String Expr
evalBinary op l r =
  case (l, r) of
   (IntValExpr il,    IntValExpr ir)    ->
     Right . IntValExpr $ intOp op il ir
   (FloatValExpr fl,  FloatValExpr fr)  ->
     do
       opFn <- floatOp op
       return $ FloatValExpr (opFn fl fr)
   (StringValExpr sl, StringValExpr sr) ->
     case op of
       Plus -> Right . StringValExpr $ sl ++ sr
       _    -> Left $ "Can't apply op " ++ (display op) ++ " to strings"
   _ -> Left $ "Can't apply binary op " ++ (display op) ++ " to " ++ (show l) ++ " and " ++ (show r)

intOp :: BinaryOp -> Int -> Int -> Int
intOp Plus   = (+)
intOp Minus  = (-)
intOp Times  = (*)
intOp Divide = div
intOp Mod    = mod
intOp Power  = (^)

floatOp :: BinaryOp -> Either String (Float -> Float -> Float)
floatOp Plus   = Right (+)
floatOp Minus  = Right (-)
floatOp Times  = Right (*)
floatOp Divide = Right (/)
floatOp Mod    = Left "Can't apply mod to floats"
floatOp Power  = Right (**)
