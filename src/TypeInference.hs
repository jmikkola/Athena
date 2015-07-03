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

{- Add a replacement to the existing set of replacements -}
addReplacement :: TypeVar -> TypeVar -> Replacements -> Replacements
addReplacement replaced replacement replacements = Map.insert replaced replacement updated
  -- replace anything referencing the replaced variable with the replacement
  where updated = Map.map (\tvar -> if tvar == replaced then replacement else tvar) replacements
-- TODO: what if `replaced` was already mapped to something? Will that ever happen?

{- Gets a replacement out of the set of replacements -}
getReplacement :: TypeVar -> Replacements -> TypeVar
getReplacement tvar replacements =
  case Map.lookup tvar replacements of
   -- If the variable hasn't been replaced yet, return the variable
   Nothing -> tvar
   -- Otherwise, return its replacement
   Just t' -> t'

{-
Before defining what we know about type variables, we first have to
have a way to describe a type.
-}
newtype TypeName = TypeName String
                 deriving (Eq, Ord, Show)

{-
Type definitions are composed with N type variables, where N is the
kind of the type. (e.g. 2 for Pair[a, b])

This allows recursion in type definitions without weird traversals to
match inner types (e.g. append :: a -> List[a] -> List[a], the inner
type of list must be matched with the type of the first argument).
-}
data TypeDefinition = TypeDefinition TypeName [TypeVar]
                    deriving (Eq, Show)

{-
Now, finally, we can define what we currently know about the type
variables:
-}
type KnownTypes = Map TypeVar TypeDefinition

{-
Test functions to allow avoiding infinite loops
-}
--containsSelf :: TypeVar -> KnownTypes -> Bool
--containsSelf var kt =

{-
Example case this would need to detect:
(code)
    let c = [b]
        _ = a == b and b == c

The type of `c` contains the type of `b`. In evaluating the type of
`a == b`, the type variable for t(b) got replaced with the one for
t(a). So to detect that types b and c can't resolve, it would need to
look up the replacement for b to see that it is recursive.
-}
containsVar :: KnownTypes -> TypeVar -> TypeDefinition -> Bool
containsVar kt tvar (TypeDefinition _ subvars) = any (isVar kt tvar) subvars

isVar :: KnownTypes -> TypeVar -> TypeVar -> Bool
isVar kt tvar tested = if tvar == tested then True
                       else case Map.lookup tested kt of
                             Nothing   -> False
                             Just tdef -> containsVar kt tvar tdef

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
