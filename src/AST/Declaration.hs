module AST.Declaration where

import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type)

data Declaration
  = Let String Type Expression
  | Function String Type [String] Statement
  | TypeDef String Type
  deriving (Eq, Show)

type File = [Declaration]
