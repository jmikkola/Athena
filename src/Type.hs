module Type where

import Data.List (intercalate)

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

genName :: Type -> String
genName t = case t of
  String          -> "String"
  Float           -> "Float"
  Int             -> "Int"
  Bool            -> "Bool"
  Nil             -> "()"
  Function ats rt ->
    "Function(" ++ (intercalate "," $ map nameOf ats) ++ ")" ++ nameOf rt
  Struct fields   ->
    "Struct{" ++ (intercalate "," $ map (\(s,r) -> s ++ ":" ++ r) fields) ++ "}"
  Enum options    ->
    "Enum{" ++ (intercalate "|" $ map (\(s,o) -> s ++ o2s o) fields) ++ "}" 

o2s :: [(String, TypeRef)] -> String
o2s fields =
  "{" ++ (intercalate "," $ map (\(f,r) -> f ++ ":" ++ r)) ++ "}"
