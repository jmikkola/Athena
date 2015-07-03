module TypeInference where

import Data.Map (Map)
import qualified Data.Map as Map

-- Define the type used for type variables
newtype TypeVar = TypeVar String
                deriving (Eq, Ord, Show)

{-
When two type variables are combined, there must be a way to figure
out what a type variable in the remaining code actually referrs to.
-}
type Replacements = Map TypeVar TypeVar

emptyReplacements :: Replacements
emptyReplacements = Map.empty

addReplacement :: TypeVar -> TypeVar -> Replacements -> Replacements
addReplacement replaced replacement replacements = Map.insert replaced replacement updated
  -- replace anything referencing the replaced variable with the replacement
  where updated = Map.map (\tvar -> if tvar == replaced then replacement else tvar) replacements
-- TODO: what if `replaced` was already mapped to something? Will that ever happen?        

getReplacement :: TypeVar -> Replacements -> TypeVar
getReplacement tvar replacements =
  case Map.lookup tvar replacements of
   -- If the variable hasn't been replaced yet, return the variable
   Nothing -> tvar
   -- Otherwise, return its replacement
   Just t' -> t'

{-
The two possible relationships between type variables. Either they
actually refer to the same type (e.g. in an expression `a == b`,
expressions a and b are the exact same type), or one is at least as
specific as another (e.g. `a == foo()`, it might be possible for a to
be an Int but foo() to be Numeric).
-}
data Relationship = SameType TypeVar TypeVar
                  | InstanceOf TypeVar TypeVar
                  deriving (Eq, Show)
