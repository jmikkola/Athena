module AST.Expression where

import AST.Type (Type)

data Value
  = StrVal String
  | BoolVal Bool
  | IntVal Int
  | FloatVal Float
  | StructVal String [(String, Expression)]
  deriving (Eq, Show)

data Expression
  = Paren Expression
  | Val Value
  | Unary UnaryOp Expression
  | Binary BinOp Expression Expression
  | Call Expression [Expression]
  | Cast Type Expression
  | Var String
  | Access Expression String
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
