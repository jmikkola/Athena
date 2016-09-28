module AST.Declaration where

import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type, TypeDecl)

data Declaration
  = Let String Type Expression
  | Function String TypeDecl [String] Statement
  | TypeDef String TypeDecl
  deriving (Eq, Show)

type File = [Declaration]
