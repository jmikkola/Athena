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
  | EUnary Op Expression
  | EBinary Op Expression Expression
  | ECall Expression [Expression]
  | ECast Type Expression
  | EVariable String
  deriving (Eq, Show)

data Op
  = Plus
  | Minus
  | Times
  | Divide
  | Mod
  | Power
  | BitAnd
  | BitOr
  | BitInvert -- unary only
  | BitXor
  | BoolAnd
  | BoolOr
  | BoolNot -- unary only
  | Eq
  | NotEq
  | Less
  | LessEq
  | Greater
  | GreaterEq
  deriving (Eq, Show)

-- TODO: lshift and rshift, negate
