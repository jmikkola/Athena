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
translate (ExpressionUnary op expr) = UnaryExpr op (translate expr)

evalExpr :: EvalContext -> Expr -> Either EvalError Expr
evalExpr ctx expr =
  case expr of
    e@(IntValExpr _)    -> Right e
    e@(FloatValExpr _)  -> Right e
    e@(StringValExpr _) -> Right e
    e@(StructValExpr _) -> Right e
    e@(VarExpr varname) ->
      case Map.lookup varname ctx of
        Just value -> Right value
        Nothing    -> Left $ EvalError { message="Variable not found", expr=e }
    e@(UnaryExpr op ex)  -> do
      inner <- evalExpr ctx ex
      evalUnary ctx op inner

evalUnary :: EvalContext -> UnaryOp -> Expr -> Either EvalError Expr
evalUnary ctx op expr =
  case expr of
    e@(IntValExpr i)    ->
      case op of
        Negate -> Right . IntValExpr $ 0 - i
        Flip   -> Right . IntValExpr $ complement i
    e@(FloatValExpr f)  -> Right e
    e                   -> Left $ EvalError
                                 { message=("Can't apply unary op " ++ (display op))
                                 , expr=e
                                 }
