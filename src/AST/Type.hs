module AST.Type where

data Type
  = String
  | Float
  | Int
  | Bool
  | Nil
  | Function [Type] Type
  | TypeName String
  | Struct [(String, Type)]
  | Enum [(String, [(String, Type)])]
  deriving (Eq, Show)
