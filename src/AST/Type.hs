module AST.Type where

type Type = String

data TypeDecl
  = TypeName Type
  | Function [TypeDecl] TypeDecl
  | Struct [(String, TypeDecl)]
  | Enum [(String, EnumOption)]
  deriving (Eq, Ord, Show)

type EnumOption = [(String, TypeDecl)]
