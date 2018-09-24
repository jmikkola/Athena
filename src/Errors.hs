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
  | CannotCast String -- a message
  | UndefinedVar String -- variable name
  | UndefinedType String -- type name
  | NonStructureType String -- type name
  | StructFieldErr String String -- type name, message
  | InvalidAnonStructure
  | Unreachable String -- function name
  | MissingReturn String -- function name
  deriving (Show, Eq)

type Result a = Either Error a
