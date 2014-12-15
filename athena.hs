{-# LANGUAGE MultiParamTypeClasses #-}

import Data.Bits (complement, shift, xor, (.&.), (.|.));

class Evaluable a b where
  eval :: a -> b

data UnaryIntOp = Negate
                | Invert
                deriving (Eq, Show)

data BinaryIntOp = Add
                 | Subtract
                 | Multiply
                 | Divide
                 | Power
                 | LeftShift
                 | RightShift
                 | And
                 | Or
                 | Xor
                 deriving (Eq, Show)

data IntExpression = IntValue Int
                   | UnaryOp UnaryIntOp IntExpression
                   | BinaryOp BinaryIntOp IntExpression IntExpression
                   deriving (Eq, Show)

evalUnaryIntOp :: UnaryIntOp -> Int -> Int
evalUnaryIntOp Negate i = (-i)
evalUnaryIntOp Invert i = complement i

evalBinaryIntOp :: BinaryIntOp -> Int -> Int -> Int
evalBinaryIntOp op left right = case op of
  Add        -> left + right
  Subtract   -> left - right
  Multiply   -> left * right
  Divide     -> left `div` right
  Power      -> left ^ right
  LeftShift  -> left `shift` right
  RightShift -> left `shift` (-right)
  And        -> left .&. right
  Or         -> left .|. right
  Xor        -> left `xor` right

evalIntExpr :: IntExpression -> Int
evalIntExpr expr = case expr of
  (IntValue i) -> i
  (UnaryOp op subexpr) -> evalUnaryIntOp op (evalIntExpr subexpr)
  (BinaryOp op leftsub rightsub) -> evalBinaryIntOp op (evalIntExpr leftsub) (evalIntExpr rightsub)

instance Evaluable IntExpression Int where
  eval = evalIntExpr

main = putStrLn $ show $ evalIntExpr $ BinaryOp Multiply (BinaryOp Add (IntValue 5) (IntValue 4)) (UnaryOp Negate (IntValue 100))
