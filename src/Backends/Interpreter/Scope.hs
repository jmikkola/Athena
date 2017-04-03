module Backends.Interpreter.Scope where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

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

type ScopeResult = Either ScopeError

empty :: Scopes a
empty = RootScope Map.empty

introduce :: Scopes a -> [(String, a)] -> ScopeResult (Scopes a)
introduce s bindings = declareAll (new s) bindings

declareAll :: Scopes a -> [(String, a)] -> ScopeResult (Scopes a)
declareAll s bindings = foldM bind s bindings
  where bind ss (n, v) = declare ss n v

declare :: Scopes a -> String -> a -> ScopeResult (Scopes a)
declare scopes name value =
  if Map.member name (current scopes)
  then Left $ DuplicateDecl name
  else return $ modifyTop scopes (Map.insert name value)

update :: Scopes a -> String -> a -> ScopeResult (Scopes a)
update scopes name value =
  if Map.member name (current scopes)
  then return $ modifyTop scopes (Map.insert name value)
  else case parent scopes of
        Nothing  -> Left $ MissingVar name
        Just par -> update par name value

get :: (Scopes a) -> String -> ScopeResult a
get scopes name = case Map.lookup name (current scopes) of
  Just val -> return val
  Nothing  -> case parent scopes of
    Just par -> get par name
    Nothing  -> Left (MissingVar name)

new :: Scopes a -> Scopes a
new s = NestedScope Map.empty s
