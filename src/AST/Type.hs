module AST.Type where

data Type
  = String
  | Float
  | Int
  | Bool
  | Nil
  | Function [Type] Type
  | TypeName String
  | StructType [(String, Type)]
  deriving (Eq, Show)
