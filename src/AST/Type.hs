module AST.Type where

data Type
  = String
  | Float
  | Int
  | Bool
  | Nil
  deriving (Eq, Show)
