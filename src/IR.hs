module IR where

import AST.Expression
  ( UnaryOp (..)
  , BinOp (..)
  )

data Statement
  = Return (Maybe Expression)
    -- Let handles both variable and function declarations
  | Let String Type Expression
  | Assign [String] Expression
  | Block [Statement]
  | Expr Expression
  | If Expression [Statement] (Maybe Statement)
  | While Expression [Statement]
  deriving (Eq, Show)

class Typeable a where
  typeOf :: a -> Type

data Value
  = StrVal String
  | BoolVal Bool
  | IntVal Int
  | FloatVal Float
  | StructVal String [(String, Expression)]
  deriving (Eq, Show)

instance Typeable Value where
  typeOf (StrVal _)      = Named "String"
  typeOf (BoolVal _)     = Named "Bool"
  typeOf (IntVal _)      = Named "Int"
  typeOf (FloatVal _)    = Named "Float"
  typeOf (StructVal n _) = Named n

data Expression
  = Paren Expression Type
  | Val Value
  | Unary Type UnaryOp Expression
  | Binary Type BinOp Expression Expression
  | Call Type Expression [Expression]
  | Cast Type Expression
  | Var Type String
  | Access Type Expression String
  | Lambda Type [String] Statement
  deriving (Eq, Show)

instance Typeable Expression where
  typeOf (Paren _ t)      = t
  typeOf (Val v)          = typeOf v
  typeOf (Unary t _ _)    = t
  typeOf (Binary t _ _ _) = t
  typeOf (Call t _ _)     = t
  typeOf (Cast t _)       = t
  typeOf (Var t _)        = t
  typeOf (Access t _ _)   = t
  typeOf (Lambda t _ _)   = t

type TypeName = String

data Type
  = Named TypeName
  | Function [Type] Type
  | Struct [(String, Type)]
  | Enum [(TypeName, [(String, Type)])]
  deriving (Eq, Ord, Show)
