module Type where

-- Idea: Just include the names directly in all types?
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

ref2named :: TypeReference -> Type
ref2named (Ref name t) = TypeName name t

refname :: TypeReference -> String
refname (Ref name _) = name
