module AST.Statement where

import AST.Expression (Expression)
import AST.Type (Type)

data Statement
  = Return (Maybe Expression)
  | Let String Type Expression
  | Assign [String] Expression
  | Block [Statement]
  | Expr Expression -- e.g. just calling a function
  | If Expression [Statement] (Maybe Statement)
  | While Expression [Statement]
  deriving (Eq, Show)
