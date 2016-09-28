module IR where

import AST.Expression
  ( UnaryOp (..)
  , BinOp (..)
  )
import Type (Type, TypeReference)
import qualified Type as T

data Decl
  = StmtDecl Statement
  | TypeDecl String Type
  deriving (Eq, Show)

data Statement
  = Return (Maybe Expression)
    -- Let handles both variable and function declarations
  | Let String TypeReference Expression
  | Assign [String] Expression
  | Block (Maybe Type) [Statement]
  | Expr Expression
  | If Expression Statement (Maybe Statement)
  | While Expression Statement
  deriving (Eq, Show)

class Typeable a where
  typeOf :: a -> Type

data Value
  = StrVal String
  | BoolVal Bool
  | IntVal Int
  | FloatVal Float
  | StructVal TypeReference [(String, Expression)]
  deriving (Eq, Show)

instance Typeable Value where
  typeOf (StrVal _)      = T.String
  typeOf (BoolVal _)     = T.Bool
  typeOf (IntVal _)      = T.Int
  typeOf (FloatVal _)    = T.Float
  typeOf (StructVal t _) =
    let Ref _ typ = t in T.TypeName n

data Expression
  = Paren Expression Type
  | Val Value
  | Unary Type UnaryOp Expression
  | Binary Type BinOp Expression Expression
  | Call Type Expression [Expression]
  | Cast TypeReference Expression
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
