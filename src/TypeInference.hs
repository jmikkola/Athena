module TypeInference where

import Control.Monad (foldM)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Graph (Graph, Vertex)
import qualified Data.Graph as Graph
import Data.Array ((!))
import qualified Data.Tree as Tree

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

data GenericGraph = GenericGraph { gg :: Graph
                                 , v2k :: (Vertex -> (TypeVar, TypeVar, [TypeVar]))
                                 , k2v :: (TypeVar -> Maybe Vertex)
                                 }

collectEdges :: Ord a => [(a, a)] -> Map a [a]
collectEdges edges = foldl addEdge Map.empty edges
  where addEdge m (start, end) = updateDefault [] (end :) start m

makeGraph :: [(TypeVar, TypeVar)] -> GenericGraph
makeGraph edges = let edges' = [(n, n, es) | (n, es) <- Map.toList $ collectEdges edges]
                      (g, vf, kf) = Graph.graphFromEdges edges'
                  in GenericGraph { gg = g, v2k = vf, k2v = kf }

findSCC :: GenericGraph -> [[TypeVar]]
findSCC g = map (map getTV . Tree.flatten) (Graph.scc (gg g))
  where vf = v2k g
        getTV vertex = let (var, _, _) = vf vertex in var

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
type GenericRules = [(TypeVar, TypeVar)]

data Rules = Rules { equalPairs :: EqualityRules
                   , specifiedTypes :: [(TypeVar, TypeNode)]
                   , genericRelations :: GenericRules }
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
infer rules = do
  (types, subs) <- collapseEqual rules
  applyGenerics (genericRelations rules) types subs

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

equalityPairsFromSet :: (Ord a) => Set a -> [(a, a)]
equalityPairsFromSet = equalityPairs . Set.toList

equalityPairs :: (Eq a) => [a] -> [(a, a)]
equalityPairs (a:ss) = zip ss (repeat a)
equalityPairs _      = []

applyGenericRules :: GenericRules -> VarTypes -> Subs -> ErrorS (VarTypes, Subs)
applyGenericRules genericPairs types subs =
  let gatherEqualPairs []         eps typs = Right (eps, typs)
      gatherEqualPairs ((i,g):gs) eps typs = do
        let eps' = walkForEqualityPairs types i g ++ eps
        let itype = Map.lookup i typs
        let gtype = Map.lookup g typs
        (result, newPairs) <- mergeMaybeGeneric itype gtype
        let typs' = case result of
              Nothing -> typs
              Just t  -> Map.insert i t typs
        gatherEqualPairs (newPairs ++ gs) eps' typs'
   in do
     (equalPairs, types') <- gatherEqualPairs genericPairs [] types
     applyEqualRules equalPairs types' subs

-- TODO: this is basically the same thing as mergeTypes
mergeGeneric :: TypeNode -> TypeNode -> ErrorS (TypeNode, GenericRules)
mergeGeneric inst general =
  let v1 = components inst
      v2 = components general
  in if (constructor inst) /= (constructor general) || (length v1) /= (length v2)
  then Left $ show inst ++ " is not a subtype of " ++ show general
  else Right (inst, zip v1 v2)

-- TODO: this is basically the same thing as mergeMaybeTypes
mergeMaybeGeneric :: Maybe TypeNode -> Maybe TypeNode -> ErrorS (Maybe TypeNode, GenericRules)
mergeMaybeGeneric (Just t1) (Just t2) = do
  (tn, gr) <- mergeTypes t1 t2
  return (Just tn, gr)
mergeMaybeGeneric Nothing   t2        = Right (t2, [])
mergeMaybeGeneric t1        Nothing   = Right (t1, [])

{-
If two different type variables end up being a generic instance of the same
generic type variable, they need to be equal.

E.g.
id :: a -> a
let c = id d

the types of d and c must equal.

This function looks for such cases and returns rules to apply.
-}
walkForEqualityPairs :: VarTypes -> TypeVar -> TypeVar -> EqualityRules
walkForEqualityPairs types instVar genVar =
  makeEqualityGroups $ gatherMappings types instVar genVar

-- Gathers a map from generic var to the set of instance vars that map to it
gatherMappings :: VarTypes -> TypeVar -> TypeVar -> Map TypeVar (Set TypeVar)
gatherMappings types instVar genVar = gm instVar genVar Map.empty
  where gm ivar gvar gmappings =
          let gmappings' = updateDefault Set.empty (Set.insert ivar) gvar gmappings
          in case (Map.lookup ivar types, Map.lookup gvar types) of
            -- recursively gather mappings when both variables map to a type
            (Just itype, Just gtype) ->
              foldl (\gmps (i, g) -> gm i g gmps) gmappings'
                    (zip (components itype) (components gtype))
            _                        -> gmappings'

makeEqualityGroups :: Map TypeVar (Set TypeVar) -> EqualityRules
makeEqualityGroups gmappings =
  concatMap equalityPairsFromSet $ Map.elems gmappings

updateDefault :: Ord k => a -> (a -> a) -> k -> Map k a -> Map k a
updateDefault defaultVal updateFn = Map.alter alterF
  where alterF existing = Just $ updateFn $ fromMaybe defaultVal existing

getDefault :: Ord k => a -> k -> Map k a -> a
getDefault def key m = case Map.lookup key m of
  Nothing -> def
  Just v  -> v

pickGenericPairs :: Map TypeVar [TypeVar] -> [[TypeVar]] -> GenericRules
pickGenericPairs graph subcomponents =
  let gatherChildren var = [(var, child) | child <- getDefault [] var graph]
  in concatMap (concatMap gatherChildren) subcomponents

applyGenerics :: GenericRules -> VarTypes -> Subs -> ErrorS (VarTypes, Subs)
applyGenerics grules types subs =
  let subbedGRs = [(applySubs subs i, applySubs subs g) | (i, g) <- grules]
      subcomps = findSCC (makeGraph subbedGRs)
      gpairs = pickGenericPairs (collectEdges subbedGRs) subcomps
  in do
    (types', subs') <- mergeSubcomponents subcomps types subs
    applyGenericRules gpairs types' subs'

mergeSubcomponents :: [[TypeVar]] -> VarTypes -> Subs -> ErrorS (VarTypes, Subs)
mergeSubcomponents []     types subs = Right (types, subs)
mergeSubcomponents (s:ss) types subs = do
  (types', subs') <- applyEqualRules (equalityPairs s) types subs
  mergeSubcomponents ss types' subs'
