module AST.Declaration where

import AST.Expression (Expression)
import AST.Statement (Statement)
import AST.Type (Type)

data Declaraction
  = Let String Expression
  | Function [(String, Type)] Type [Statement]
  deriving (Eq, Show)

type File = [Declaraction]
