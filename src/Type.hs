module Type where

import Data.List (intercalate)

data Type
  = String
  | Float
  | Int
  | Bool
  | Nil
  | Function [Type] Type
  | Struct String [(String, Type)]
  | Enum String [(String, [(String, Type)])]
  deriving (Eq, Ord, Show)

name :: Type -> String
name t = case t of
  String -> "String"
  Float -> "Float"
  Int -> "Int"
  Bool -> "Bool"
  Nil -> "()"
  Function ats rt -> "Function(" ++ (intercalate "," $ map name ats) ++ ")" ++ name rt
  Struct n _ -> n
  Enum n _ -> n
