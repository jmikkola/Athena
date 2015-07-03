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
containsSelf :: KnownTypes -> TypeVar -> Bool
containsSelf kt var = varContainsVar kt var var

containsVar :: KnownTypes -> TypeVar -> TypeDefinition -> Bool
containsVar kt tvar (TypeDefinition _ subvars) = any (isVar kt tvar) subvars

-- Helper function
isVar :: KnownTypes -> TypeVar -> TypeVar -> Bool
isVar kt tvar tested = if tvar == tested then True
                       else varContainsVar kt tvar tested

-- Helper function
varContainsVar :: KnownTypes -> TypeVar -> TypeVar -> Bool
varContainsVar kt tvar tested = case Map.lookup tested kt of
  Nothing   -> False
  Just tdef -> containsVar kt tvar tdef


{-
Now to actually do some of the real work of merging types.

This doesn't (yet) handle interfaces, so it would fail to merge String
and Eq, for example.
-}
mergeEqual :: KnownTypes -> Replacements -> TypeVar -> TypeVar ->
             Either String (KnownTypes, Replacements, [Relationship])
mergeEqual kt replacements varA varB =
  let repA = getReplacement varA replacements
      repB = getReplacement varB replacements
  in if repA == repB
        -- Handle the silly case where the two equal
     then return (kt, replacements, [])
     else let typeA = Map.lookup repA kt
              typeB = Map.lookup repB kt
          in case (typeA, typeB) of
              (Nothing, Nothing) -> return (kt, addReplacement repA repB replacements, [])
              (Just _,  Nothing) -> return (kt, addReplacement repB repA replacements, [])
              (Nothing, Just _)  -> return (kt, addReplacement repA repB replacements, [])
              (Just da, Just db) -> mergeTypes kt replacements repA da repB db

mergeTypes :: KnownTypes -> Replacements -> TypeVar -> TypeDefinition -> TypeVar -> TypeDefinition
           -> Either String (KnownTypes, Replacements, [Relationship])
mergeTypes kt replacements varA tdefA varB tdefB =
  let (TypeDefinition typeA subtypesA) = tdefA
      (TypeDefinition typeB subtypesB) = tdefB
  in if typeA /= typeB
     then Left $ "Can't merge incompatible types " ++ show typeA ++ " and " ++ show typeB
     else if length subtypesA /= length subtypesB
          then Left $ "Compiler error: subtype lengths should match: " ++
                      show subtypesA ++ ", " ++ show subtypesB
          else let remainingRelationships = requireSame subtypesA subtypesB
                   replacements' = addReplacement varB varA replacements
                   kt' = Map.delete varB kt
               in return (kt', replacements', remainingRelationships)

-- Helper function
requireSame :: [TypeVar] -> [TypeVar] -> [Relationship]
requireSame (x:xs) (y:ys) = (SameType x y) : requireSame xs ys
requireSame _      _      = []

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
