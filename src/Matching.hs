module Matching where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State

import IR
import Type

data Coverage
  = TotalCoverage
  | Partial (Map String Coverage)
  deriving (Eq, Show)

data MatchError
  = Unreachable String
  | DuplicateName String
  | Incomplete
  deriving (Eq, Show)

type MatchResult = Either MatchError

type EnumOption = (String, [(String, TypeRef)])

checkMatchExpressions :: TypeCheckState -> [IR.MatchExpression] -> Maybe MatchError
checkMatchExpressions types exprs = case checkExpressions types exprs of
  Left  err           -> Just err
  Right (Partial _)   -> Just Incomplete
  Right TotalCoverage -> Nothing

checkExpressions :: TypeCheckState -> [IR.MatchExpression] -> MatchResult Coverage
checkExpressions types exprs = do
  _ <- mapM (\e -> evalStateT (duplicatesCheck e) Set.empty) exprs
  foldM (addCoverage types) noCoverage exprs

type VarUseState = StateT (Set String) MatchResult

-- Check for duplicate names
duplicatesCheck :: IR.MatchExpression -> VarUseState ()
duplicatesCheck matchExpr = case matchExpr of
  MatchAnything           -> return ()
  MatchVariable v         -> addVar v
  MatchStructure _ fields -> do
    _ <- mapM duplicatesCheck fields
    return ()

-- Mark a variable as used, failing if it has already been used
addVar :: String -> VarUseState ()
addVar v = do
  existing <- get
  if Set.member v existing
    then lift . Left . DuplicateName $ "variable name used twice in the same match case: " ++ v
    else put $ Set.insert v existing

noCoverage :: Coverage
noCoverage = Partial Map.empty

unreachable :: String -> MatchResult a
unreachable = Left . Unreachable

addCoverage :: TypeCheckState -> Coverage -> IR.MatchExpression -> MatchResult Coverage
addCoverage types coverage expr = do
  exprCoverage <- genTree types expr
  mustAddCoverage coverage exprCoverage

mustAddCoverage :: Coverage -> Coverage -> MatchResult Coverage
mustAddCoverage existing new =
  let merged = mergeCoverage existing new
  in if merged == existing
     then unreachable $ show new
     else return merged

genTree :: TypeCheckState -> IR.MatchExpression -> MatchResult Coverage
genTree types expr = case expr of
  MatchAnything   ->
    return TotalCoverage
  MatchVariable _ ->
    return TotalCoverage
  MatchStructure typ fields -> do
    fieldNames <- getStructFieldNames types typ
    enumType <- getEnumFromOption types typ
    options <- getEnumOptions types enumType

    fieldTrees <- mapM (genTree types) fields
    let fieldCoverage = Partial $ Map.fromList $ zip fieldNames fieldTrees
    let emptyTree = partialForEnum options
    return $ mergeCoverage emptyTree fieldCoverage

getStructFieldNames :: TypeCheckState -> TypeRef -> MatchResult [String]
getStructFieldNames tcs typ = case Map.lookup typ (types tcs) of
  Nothing -> fail "uknown type in getStructFieldNames"
  Just t  -> case t of
    Struct fields -> return $ map fst fields
    _             -> fail "invalid type in getStructFieldNames"


getEnumFromOption :: TypeCheckState -> TypeRef -> MatchResult TypeRef
getEnumFromOption tcs typ = case Map.lookup typ (enumParents tcs) of
  Nothing -> fail "unknown enum variant in getEnumFromOption"
  Just t  -> return t

getEnumOptions :: TypeCheckState -> TypeRef -> MatchResult [EnumOption]
getEnumOptions tcs typ = case lookupEnumOptions tcs typ of
  Nothing   -> fail "error in getEnumOptions"
  Just opts -> return opts

lookupEnumOptions :: TypeCheckState -> TypeRef -> Maybe [EnumOption]
lookupEnumOptions tcs typ = do
  variants <- Map.lookup typ (enumVariants tcs)
  mapM (\t -> Map.lookup t (types tcs)) (Set.toList variants)

partialForEnum :: [EnumOption] -> Coverage
partialForEnum options =
  merge $ map (uncurry partialForVariant) options

partialForVariant :: String -> [(String, TypeRef)] -> Coverage
partialForVariant enumName fields =
  partialOf enumName $ merge $ map partialForField $ map fst fields

partialForField :: String -> Coverage
partialForField name = partialOf name noCoverage

partialOf :: String -> Coverage -> Coverage
partialOf s c = Partial $ Map.singleton s c

merge :: [Coverage] -> Coverage
merge = foldl mergeCoverage noCoverage

mergeCoverage :: Coverage -> Coverage -> Coverage
mergeCoverage _ TotalCoverage = TotalCoverage
mergeCoverage TotalCoverage _ = TotalCoverage
mergeCoverage (Partial m1) (Partial m2) =
  Partial $ Map.unionWith mergeCoverage m1 m2
