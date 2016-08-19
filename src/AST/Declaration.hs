module AST.Declaration where

import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type)

data Declaraction
  = Let String Type Expression
  | Function String Type [String] Statement
  deriving (Eq, Show)

type File = [Declaraction]
