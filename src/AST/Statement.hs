module AST.Statement where

import AST.Annotation (Annotation, Annotated, getAnnotation, setAnnotation)
import AST.Expression (Expression)
import AST.Type (Type)

data Statement
  = Return  Annotation (Maybe Expression)
  | Let     Annotation String (Maybe Type) Expression
  | Assign  Annotation [String] Expression
  | Block   Annotation [Statement]
  | Expr    Annotation Expression -- e.g. just calling a function
  | If      Annotation Expression [Statement] (Maybe Statement)
  | While   Annotation Expression [Statement]
  | Match   Annotation Expression [MatchCase]
  deriving (Eq, Show)

data MatchCase
  = MatchCase MatchExpression Statement
  deriving (Eq, Show)

-- TODO: add support for matching literal values (int, string)
data MatchExpression
  = MatchAnything  Annotation
  | MatchVariable  Annotation String
  | MatchStructure Annotation String [MatchExpression]
  deriving (Eq, Show)

instance Annotated Statement where
  getAnnotation stmt = case stmt of
    Return a _     -> a
    Let    a _ _ _ -> a
    Assign a _ _   -> a
    Block  a _     -> a
    Expr   a _     -> a
    If     a _ _ _ -> a
    While  a _ _   -> a
    Match  a _ _   -> a

  setAnnotation ann stmt = case stmt of
    Return _ e     -> Return ann e
    Let    _ n t e -> Let    ann n t e
    Assign _ n e   -> Assign ann n e
    Block  _ s     -> Block  ann s
    Expr   _ e     -> Expr   ann e
    If     _ t s e -> If     ann t s e
    While  _ e s   -> While  ann e s
    Match  _ e m   -> Match  ann e m

instance Annotated MatchCase where
  getAnnotation (MatchCase expr _) = getAnnotation expr

  setAnnotation ann (MatchCase expr stmt) =
    MatchCase (setAnnotation ann expr) stmt

instance Annotated MatchExpression where
  getAnnotation matchExpr = case matchExpr of
    MatchAnything  a     -> a
    MatchVariable  a _   -> a
    MatchStructure a _ _ -> a

  setAnnotation ann matchExpr = case matchExpr of
    MatchAnything _       -> MatchAnything ann
    MatchVariable _ v     -> MatchVariable ann v
    MatchStructure _ s me -> MatchStructure ann s me
