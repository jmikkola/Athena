module AST.Declaration where

import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type, TypeDecl)

data Declaration
  = Let String Type Expression
  | Function String TypeDecl [String] Statement
  | TypeDef String TypeDecl
  deriving (Eq, Show)

getDeclaredName :: Declaration -> String
getDeclaredName (Let name _ _)        = name
getDeclaredName (Function name _ _ _) = name
getDeclaredName (TypeDef name _)      = name

type File = [Declaration]
