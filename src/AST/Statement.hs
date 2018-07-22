module AST.Statement where

import AST.Expression (Expression)
import AST.Type (Type)

data Statement a
  = Return a (Maybe (Expression a))
  | Let a String Type (Expression a)
  | Assign a [String] (Expression a)
  | Block a [Statement a]
  | Expr a (Expression a) -- e.g. just calling a function
  | If a (Expression a) [Statement a] (Maybe (Statement a))
  | While a (Expression a) [Statement a]
  -- | Match a (Expression a) [MatchCase a]
  deriving (Eq, Show)

{-
data MatchCase a
  = MatchCase (MatchExpression a) (Statement a)
  deriving (Eq, Show)

-- TODO: add support for matching literal values (int, string)
data MatchExpression a
  = MatchAnything a
  | MatchVariable a String
  | MatchStructure a String [(MatchExpression a)]
  deriving (Eq, Show)
-}
