module TypeInference where

import Data.Map (Map)
import qualified Data.Map as Map

type ErrorS = Either String

type TypeVar = Int

-- Internal representation of a type
data TypeNode = TypeNode { constructor :: String
                         , components :: [TypeVar] }
              deriving (Show, Eq, Ord)

-- External representation of a type
data Type = Constructor String [Type] | Var TypeVar
          deriving (Show, Eq, Ord)

type VarTypes = Map TypeVar TypeNode
-- Substitutions
type Subs = Map TypeVar TypeVar
type InfResult = (VarTypes, Subs)

applySubs :: Subs -> TypeVar -> TypeVar
applySubs subs var = case Map.lookup subs var of
  Nothing -> var
  Just v' -> v'

getTypeForVar :: InfResult -> TypeVar -> Maybe TypeNode
getTypeForVar (types, subs) var = Map.lookup types (applySubs subs var)
