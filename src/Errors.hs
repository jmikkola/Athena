module Errors where

-- TODO: create a module for file position
-- and then annotate errors with where they came form.

import Types (Type)

data Error
  = Mismatch Type Type
  | InfiniteType String -- a type variable
  | CompilerBug String
  | DuplicateBinding String -- binding name
  | ParseError String
  deriving (Show, Eq)

type Result a = Either Error a
