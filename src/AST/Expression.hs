module AST.Expression where

import AST.Annotation (Annotated, getAnnotation)
import AST.Type (Type)

data Value a
  = StrVal     a String
  | BoolVal    a Bool
  | IntVal     a Int
  | FloatVal   a Float
  | StructVal  a String [(String, Expression a)]
  deriving (Eq, Show)

data Expression a
  = Paren   a (Expression a)
  | Val     a (Value a)
  | Unary   a UnaryOp (Expression a)
  | Binary  a BinOp (Expression a) (Expression a)
  | Call    a (Expression a) [(Expression a)]
  | Cast    a Type (Expression a)
  | Var     a String
  | Access  a (Expression a) String
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
  deriving (Eq, Show)

instance Annotated Value where
  getAnnotation val = case val of
    StrVal    a _   -> a
    BoolVal   a _   -> a
    IntVal    a _   -> a
    FloatVal  a _   -> a
    StructVal a _ _ -> a

instance Annotated Expression where
  getAnnotation expr = case expr of
    Paren   a _     -> a
    Val     a _     -> a
    Unary   a _ _   -> a
    Binary  a _ _ _ -> a
    Call    a _ _   -> a
    Cast    a _ _   -> a
    Var     a _     -> a
    Access  a _ _   -> a
