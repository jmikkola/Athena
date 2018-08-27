module AST.Statement where

import AST.Annotation (Annotated, getAnnotation)
import AST.Expression (Expression)
import AST.Type (Type)

-- TODO: Bring back type annotations later

data Statement a
  = Return  a (Maybe (Expression a))
  | Let     a String {- Type -} (Expression a)
  | Assign  a [String] (Expression a)
  | Block   a [Statement a]
  | Expr    a (Expression a) -- e.g. just calling a function
  | If      a (Expression a) [Statement a] (Maybe (Statement a))
  | While   a (Expression a) [Statement a]
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

instance Annotated Statement where
  getAnnotation stmt = case stmt of
    Return a _     -> a
    Let    a _ _   -> a
    Assign a _ _   -> a
    Block  a _     -> a
    Expr   a _     -> a
    If     a _ _ _ -> a
    While  a _ _   -> a
