module AST.Expression where

import AST.Type (Type)

data Value
  = EString String
  | EBool Bool
  | EInt Int
  | EFloat Float
  deriving (Eq, Show)

data Expression
  = EParen Expression
  | EValue Value
  | EUnary UnaryOp Expression
  | EBinary BinOp Expression Expression
  | ECall Expression [Expression]
  | ECast Type Expression
  | EVariable String
  deriving (Eq, Show)

data UnaryOp
  = BitInvert
  | BoolNot
  deriving (Eq, Show)

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Power
  | BitAnd
  | BitOr
  | BitXor
  | BoolAnd
  | BoolOr
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  | LShift
  | RShift
  | RRShift
  deriving (Eq, Show)
