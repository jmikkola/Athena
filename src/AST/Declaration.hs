module AST.Declaration where

import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type, TypeDecl)

data Declaration a
  = Let a String Type (Expression a)
  | Function a String TypeDecl [String] (Statement a)
  | TypeDef a String TypeDecl
  deriving (Eq, Show)

getDeclaredName :: Declaration a -> String
getDeclaredName (Let _ name _ _)        = name
getDeclaredName (Function _ name _ _ _) = name
getDeclaredName (TypeDef _ name _)      = name

type File a = [Declaration a]
