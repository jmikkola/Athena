module IR.Type where

type TypeName = String

data Type
  = Named TypeName
  | Function [Type] Type
  | TypeName String
  | Struct [(String, Type)]
  | Enum [(TypeName, [(String, Type)])]
  deriving (Eq, Ord, Show)
