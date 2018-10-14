module Errors where

import Data.List (intercalate)

import Region

-- TODO: create a module for file position
-- and then annotate errors with where they came form.

import Types (Type)

data Error
  = Mismatch Type Type
  | WrongType Type String
  | InfiniteType String -- a type variable
  | BindingTooGeneral String -- name of binding
  | CompilerBug String
  | DuplicateBinding String -- binding name
  | ParseError String
  | CannotCast String -- a message
  | UndefinedVar String -- variable name
  | UndefinedField String String -- struct name, field name
  | UndefinedType String -- type name
  | NonStructureType String -- type name
  | StructFieldErr String String -- type name, message
  | PatternErr String -- message
  | InvalidAnonStructure
  | InsufficientlyDefinedType
  | Unreachable String -- function name
  | MissingReturn String -- function name
  | WithLocations [Region] Error -- wrap another error, adding a location
  deriving (Show, Eq)

type Result a = Either Error a

renderError :: Error -> String -> String
renderError err fileContent = case err of
  WithLocations regions err' ->
    addRegions regions fileContent (renderError err' fileContent)
  Unreachable fname ->
    "unreachable code in function " ++ fname
  _ ->
    show err


-- This is a very suboptimal way to do this, but it
-- works for now
addRegions :: [Region] -> String -> String -> String
addRegions regions fileContent errStr =
  let fileLines = lines fileContent
      fileLocations = map (showRegion fileLines) regions
  in intercalate "\n" (errStr : fileLocations)
