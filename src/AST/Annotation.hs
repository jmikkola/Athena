module AST.Annotation where

class Annotated a where
  getAnnotation :: a ann -> ann
