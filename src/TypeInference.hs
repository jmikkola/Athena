module TypeInference where

import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map

type ErrorS = Either String

type TypeVar = Int
type VarGen = [TypeVar]

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

genTypeVars :: VarGen
genTypeVars = [0..]

applySubs :: Subs -> TypeVar -> TypeVar
applySubs subs var = case Map.lookup var subs of
  Nothing -> var
  Just v' -> v'

getTypeForVar :: InfResult -> TypeVar -> Maybe TypeNode
getTypeForVar (types, subs) var = Map.lookup (applySubs subs var) types

getFullTypeForVar :: InfResult -> TypeVar -> Maybe Type
getFullTypeForVar ir@(types, subs) var =
  let fullTypeOrVar v = fromMaybe (Var v) (getFullTypeForVar ir v)
  in do
    node <- getTypeForVar ir var
    let subtypes = map fullTypeOrVar (components node)
    return $ Constructor (constructor node) subtypes
