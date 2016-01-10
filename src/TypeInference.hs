module TypeInference where

import Control.Monad (foldM)
import Data.Maybe (fromMaybe, isJust, isNothing)
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
getFullTypeForVar ir var =
  let fullTypeOrVar v = fromMaybe (Var v) (getFullTypeForVar ir v)
  in do
    node <- getTypeForVar ir var
    let subtypes = map fullTypeOrVar (components node)
    return $ Constructor (constructor node) subtypes

type EqualityRules = [(TypeVar, TypeVar)]

data Rules = Rules { equalPairs :: EqualityRules
                   , specifiedTypes :: [(TypeVar, TypeNode)]
                   , genericRelations :: [(TypeVar, TypeVar)] }
           deriving (Show)

emptyRules :: Rules
emptyRules = Rules [] [] []

setEqual :: TypeVar -> TypeVar -> Rules -> Rules
setEqual a b rules = rules { equalPairs = (a, b) : equalPairs rules }

specify :: TypeVar -> TypeNode -> Rules -> Rules
specify tv tn rules = rules { specifiedTypes = (tv, tn) : specifiedTypes rules }

instanceOf :: TypeVar -> TypeVar -> Rules -> Rules
instanceOf inst general rules = rules { genericRelations = g' }
  where g' = (inst, general) : genericRelations rules

infer :: Rules -> ErrorS InfResult
-- TODO: add generics
infer rules = collapseEqual rules

mergeTypes :: TypeNode -> TypeNode -> ErrorS (TypeNode, EqualityRules)
mergeTypes t1 t2 =
  let v1 = components t1
      v2 = components t2
  in if (constructor t1) /= (constructor t2) || (length v1) /= (length v2)
  then Left $ show t1 ++ " is not compatible with " ++ show t2
  else Right (t1, zip v1 v2)

mergeMaybeTypes :: Maybe TypeNode -> Maybe TypeNode -> ErrorS (Maybe TypeNode, EqualityRules)
mergeMaybeTypes (Just t1) (Just t2) = do
  (tn, er) <- mergeTypes t1 t2
  return (Just tn, er)
mergeMaybeTypes Nothing   t2        = Right (t2, [])
mergeMaybeTypes t1        Nothing   = Right (t1, [])

-- TODO: test this
collapseEqual :: Rules -> ErrorS (VarTypes, Subs)
collapseEqual rules = do
  (ts, extra_er) <- collapseSpecifiedTypes rules
  applyEqualRules (extra_er ++ equalPairs rules) ts Map.empty

collapseSpecifiedTypes :: Rules -> ErrorS (VarTypes, EqualityRules)
collapseSpecifiedTypes rules =
  foldM addSpecified (Map.empty, []) (specifiedTypes rules)
  where addSpecified (ts, er) (tv, tn) =
          case Map.lookup tv ts of
            Nothing  -> Right (Map.insert tv tn ts, [])
            Just tn' -> do
              (tn'', rules) <- mergeTypes tn tn'
              return (Map.insert tv tn'' ts, rules ++ er)

applyEqualRules :: EqualityRules -> VarTypes -> Subs -> ErrorS (VarTypes, Subs)
applyEqualRules []     vars subs = Right (vars, subs)
applyEqualRules (r:rs) vars subs = do
  (newRules, vars, subs) <- equalRulesStep r vars subs
  applyEqualRules (newRules ++ rs) vars subs

equalRulesStep :: (TypeVar, TypeVar) -> VarTypes -> Subs -> ErrorS (EqualityRules, VarTypes, Subs)
equalRulesStep (rv1, rv2) vars subs =
  let v1 = applySubs subs rv1
      v2 = applySubs subs rv2
      type1 = Map.lookup v1 vars
      type2 = Map.lookup v2 vars
      -- Default to the first type variable that has been set to
      -- something to make the output more predictable. This doesn't
      -- actually do anything for the algorithm.
      (replaced, replacement) = if isNothing type1 && isJust type2
                                then (v1, v2) else (v2, v1)
  in do
    (result, newRules) <- mergeMaybeTypes type1 type2
    let subs' = addReplacement subs replaced replacement
    let vars' = addReplacementToTypes vars result replaced replacement
    return (newRules, vars', subs')

addReplacementToTypes vars result replaced replacement =
  let withoutReplaced = Map.delete replaced vars
      withResult = case result of
        Nothing -> Map.delete replacement withoutReplaced
        Just t  -> Map.insert replacement t withoutReplaced
  in Map.map (applySubToType replaced replacement) withResult

replace replaced replacement tv = if tv == replaced then replacement else tv

addReplacement :: Subs -> TypeVar -> TypeVar -> Subs
addReplacement oldSubs replaced replacement =
  if replaced == replacement then oldSubs
  else let subs' = Map.map (replace replaced replacement) oldSubs
       in Map.insert replaced replacement subs'

applySubToType replaced replacement (TypeNode con vs) =
  TypeNode con (map (replace replaced replacement) vs)
