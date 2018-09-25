module AST.Declaration where

import AST.Annotation (Annotated, getAnnotation)
import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type, TypeDecl)

type File a = [Declaration a]

data Declaration a
  = Let a String (Maybe Type) (Expression a)
  | Function a String (Maybe TypeDecl) [String] (Statement a)
  | TypeDef a String TypeDecl
  deriving (Eq, Show)

getDeclaredName :: Declaration a -> String
getDeclaredName (Let      _ name _ _)   = name
getDeclaredName (Function _ name _ _ _) = name
getDeclaredName (TypeDef  _ name _)     = name

instance Annotated Declaration where
  getAnnotation decl = case decl of
    Let      a _ _ _   -> a
    Function a _ _ _ _ -> a
    TypeDef  a _ _     -> a
