module IR where

import AST.Expression
  ( UnaryOp (..)
  , BinOp (..)
  )
import Type (Type, TypeRef)
--import qualified Type as T

data Decl
  = StmtDecl Statement
  | TypeDecl String Type
  deriving (Eq, Show)

data Statement
  = Return (Maybe Expression)
    -- Let handles both variable and function declarations
  | Let String TypeRef Expression
  | Assign [String] Expression
  | Block (Maybe TypeRef) [Statement]
  | Expr Expression
  | If Expression Statement (Maybe Statement)
  | While Expression Statement
  | Match Expression [MatchCase]
  deriving (Eq, Show)

data MatchCase
  = MatchCase MatchExpression Statement
  deriving (Eq, Show)

caseExpression :: MatchCase -> MatchExpression
caseExpression (MatchCase expr _) = expr

data MatchExpression
  = MatchAnything
  | MatchVariable String
  | MatchStructure TypeRef [MatchExpression]
  deriving (Eq, Show)

class Typeable a where
  typeOf :: a -> TypeRef

data Value
  = StrVal String
  | BoolVal Bool
  | IntVal Int
  | FloatVal Float
  | StructVal TypeRef [(String, Expression)]
  | LambdaVal TypeRef [String] Statement
  | EmptyValue
  deriving (Eq, Show)

instance Typeable Value where
  typeOf (StrVal _)        = "String"
  typeOf (BoolVal _)       = "Bool"
  typeOf (IntVal _)        = "Int"
  typeOf (FloatVal _)      = "Float"
  typeOf (StructVal t _)   = t
  typeOf (LambdaVal t _ _) = t
  typeOf EmptyValue        = "()"

data Expression
  = Paren Expression TypeRef
  | Val Value
  | Unary TypeRef UnaryOp Expression
  | Binary TypeRef BinOp Expression Expression
  | Call TypeRef Expression [Expression]
  | Cast TypeRef Expression
  | Var TypeRef String
  | Access TypeRef Expression String
  | Lambda TypeRef [String] Statement
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
