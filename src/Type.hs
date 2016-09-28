module Type where

data Type
  = String
  | Float
  | Int
  | Bool
  | Nil
  | Function [Type] Type
  | TypeName String Type -- Right?
  | Struct [(String, Type)]
  | Enum [(String, [(String, Type)])]
  deriving (Eq, Ord, Show)

data TypeReference
  = Ref String Type
  deriving (Eq, Ord, Show)
