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
    (UnaryExpr op ex)   -> do
      inner <- evalExpr ctx ex
      evalUnary op inner
    (BinaryExpr op l r) -> do
      left <- evalExpr ctx l
      right <- evalExpr ctx r
      evalBinary op left right

evalUnary :: UnaryOp -> Expr -> Either EvalError Expr
evalUnary op ex =
  case ex of
    (IntValExpr i)    ->
      case op of
        Negate -> Right . IntValExpr $ 0 - i
        Flip   -> Right . IntValExpr $ complement i
    (FloatValExpr f)  ->
      case op of
        Negate -> Right . FloatValExpr $ 0 - f
        _      -> Left $ EvalError
                        { message=("Can't apply unary op " ++ (display op) ++ " to a float")
                        , expr=ex
                        }
    _                 -> Left $ EvalError
                                 { message=("Can't apply unary op " ++ (display op))
                                 , expr=ex
                                 }

evalBinary :: BinaryOp -> Expr -> Expr -> Either EvalError Expr
evalBinary op l r =
  case (l, r) of
   (IntValExpr il, IntValExpr ir) ->
     case op of
      Plus   -> Right . IntValExpr $ il + ir
      Minus  -> Right . IntValExpr $ il - ir
      Times  -> Right . IntValExpr $ il * ir
      Divide -> Right . IntValExpr $ il `div` ir
      Mod    -> Right . IntValExpr $ il `mod` ir
      Power  -> Right . IntValExpr $ il ^ ir
   (FloatValExpr fl, FloatValExpr fr) ->
     case op of
      Plus   -> Right . FloatValExpr $ fl + fr
      Minus  -> Right . FloatValExpr $ fl - fr
      Times  -> Right . FloatValExpr $ fl * fr
      Divide -> Right . FloatValExpr $ fl / fr
      Mod    -> Left $ EvalError
                      { message=("Can't apply mod to floats")
                      , expr=(BinaryExpr op l r)
                      }
      Power  -> Right . FloatValExpr $ fl ** fr
   (StringValExpr sl, StringValExpr sr) ->
     case op of
       Plus -> Right . StringValExpr $ sl ++ sr
       _    -> Left $ EvalError
                          { message=("Can't apply op " ++ (display op) ++ " to strings")
                          , expr=(BinaryExpr op l r)
                          }
   _ -> Left $ EvalError
               { message=("Can't apply binary op " ++ (display op) ++ " to " ++ (show l) ++ " and " ++ (show r))
               , expr=(BinaryExpr op l r)
               }
