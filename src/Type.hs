module Type where

import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)

data Type
  = String
  | Float
  | Int
  | Bool
  | Nil
  | Function [TypeRef] TypeRef
  | Struct [(String, TypeRef)]
  | Enum [(String, [(String, TypeRef)])]
  deriving (Eq, Ord, Show)

type TypeRef = String -- name of a type

type Scope = Map String TypeRef
type TypeScope = [Scope]
type Subtypes = Map TypeRef (Set TypeRef)
type EnumVariants = Map TypeRef (Set TypeRef)
type TypeMap = Map TypeRef Type
data TypeCheckState
  = TypeCheckState
    { varScope :: [Scope]
    , types :: TypeMap
    , subtypes :: Subtypes
    , enumVariants :: EnumVariants
    }

genName :: Type -> String
genName t = case t of
  String          -> "String"
  Float           -> "Float"
  Int             -> "Int"
  Bool            -> "Bool"
  Nil             -> "()"
  Function ats rt ->
    "Function(" ++ (intercalate "," ats) ++ ")" ++ rt
  Struct fields   ->
    "Struct{" ++ (intercalate "," $ map (\(s,r) -> s ++ ":" ++ r) fields) ++ "}"
  Enum options    ->
    "Enum{" ++ (intercalate "|" $ map (\(s,o) -> s ++ o2s o) options) ++ "}"

o2s :: [(String, TypeRef)] -> String
o2s fields =
  "{" ++ (intercalate "," $ map (\(f,r) -> f ++ ":" ++ r) fields) ++ "}"
