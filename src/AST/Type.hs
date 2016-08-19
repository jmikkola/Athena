module AST.Type where

data Type
  = String
  | Float
  | Int
  | Bool
  | Nil
  | Function [Type] Type
  deriving (Eq, Show)
