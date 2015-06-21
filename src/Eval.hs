module Eval where

import Data.Map (Map)
--import qualified Data.Map as Map

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
translate (ExpressionLit literal)   = case literal of
  LiteralInt i    -> IntValExpr i
  LiteralFloat f  -> FloatValExpr f
  LiteralString s -> StringValExpr s
  LiteralStruct s -> StructValExpr s
translate (ExpressionVar name)      = VarExpr name
translate (ExpressionParen inner)   = translate inner
translate (ExpressionFnCall _ _)    = error "TODO"
translate (ExpressionBinary op l r) = BinaryExpr op (translate l) (translate r)
translate (ExpressionUnary op expr) = UnaryExpr op (translate expr)

evalExpr :: Expression -> Either EvalError Expression
evalExpr = undefined
