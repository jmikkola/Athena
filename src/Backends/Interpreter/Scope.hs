module Backends.Interpreter.Scope where

import Data.Map (Map)
import qualified Data.Map as Map

type Scope = Map String
data Scopes a
  = RootScope (Scope a)
  | NestedScope (Scope a) (Scopes a)
  deriving (Show)

current :: Scopes a -> Scope a
current (RootScope s)     = s
current (NestedScope s _) = s

parent :: Scopes a -> Maybe (Scopes a)
parent (NestedScope _ p) = Just p
parent _                 = Nothing

modifyTop :: Scopes a -> (Scope a -> Scope a) -> Scopes a
modifyTop (RootScope s)     f = RootScope (f s)
modifyTop (NestedScope s p) f = NestedScope (f s) p

data ScopeError
  = MissingVar String
  | DuplicateDecl String
  deriving (Show)

empty :: Scopes a
empty = RootScope Map.empty

declare :: Scopes a -> String -> a -> Either ScopeError (Scopes a)
declare scopes name value =
  if Map.member name (current scopes)
  then Left $ DuplicateDecl name
  else return $ modifyTop scopes (Map.insert name value)

update :: Scopes a -> String -> a -> Either ScopeError (Scopes a)
update scopes name value =
  if Map.member name (current scopes)
  then return $ modifyTop scopes (Map.insert name value)
  else case parent scopes of
        Nothing  -> Left $ MissingVar name
        Just par -> update par name value

get :: (Scopes a) -> String -> Either ScopeError a
get scopes name = case Map.lookup name (current scopes) of
  Just val -> return val
  Nothing  -> case parent scopes of
    Just par -> get par name
    Nothing  -> Left (MissingVar name)

newScope :: Scopes a -> Scopes a
newScope s = NestedScope Map.empty s
