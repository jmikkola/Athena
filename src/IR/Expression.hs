module IR.Expression where

import IR.Type (Type)
import qualified IR.Type as T

import AST.Expression
  ( UnaryOp (..)
  , BinOp (..)
  )

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
  typeOf (StrVal _)      = T.Named "String"
  typeOf (BoolVal _)     = T.Named "Bool"
  typeOf (IntVal _)      = T.Named "Int"
  typeOf (FloatVal _)    = T.Named "Float"
  typeOf (StructVal n _) = T.Named n

data Expression
  = Paren Expression Type
  | Val Value
  | Unary Type UnaryOp Expression
  | Binary Type BinOp Expression Expression
  | Call Type Expression [Expression]
  | Cast Type Expression
  | Var Type String
  | Access Type Expression String
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
