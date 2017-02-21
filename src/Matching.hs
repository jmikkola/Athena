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

checkMatchExpressions :: [IR.MatchExpression] -> Maybe MatchError
checkMatchExpressions exprs = case checkExpressions exprs of
  Left  err           -> Just err
  Right (Partial _)   -> Just Incomplete
  Right TotalCoverage -> Nothing

checkExpressions :: [IR.MatchExpression] -> MatchResult Coverage
checkExpressions exprs = do
  _ <- mapM (\e -> evalStateT (duplicatesCheck e) Set.empty) exprs
  foldM addCoverage noCoverage exprs

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

-- addCoverage :: TypeMap -> EnumVariants -> Coverage -> IR.MatchExpression -> MatchResult Coverage
addCoverage types variants coverage expr =
  mustAddCoverage coverage (genTree types variants expr)

mustAddCoverage :: Coverage -> Coverage -> MatchResult Coverage
mustAddCoverage existing new =
  let merged = mergeCoverage existing new
  in if merged == existing
     then unreachable $ show new
     else return merged

--genTree :: TypeMap -> EnumVariants -> MatchExpression
genTree types variants expr = case expr of
  MatchAnything   ->
    return TotalCoverage
  MatchVariable _ ->
    return TotalCoverage
  MatchStructure typ fields -> do
    -- TODO: get options fieldnames
    options <- case Map.lookup typ variants of -- TODO: get parents rather than the variants of the individual structure
      Nothing   -> fail $ "Comppiler bug: can't find type called " ++ typ
      Just opts -> return opts
    fieldNames <- case Map.lookup typ types of
      Nothing   -> fail $ "Comppiler bug: can't find type called " ++ typ
      Just _ -> undefined  -- TODO verify that this is a structure type and grab the field names

    fieldTrees <- mapM (genTree types variants) fields
    let fieldCoverage = Partial $ Map.fromList $ zip fieldNames fieldTrees
    let emptyTree = partialForEnum options
    return $ mergeCoverage emptyTree (partialOf typ namedFieldTrees)

partialForEnum :: [EnumOption] -> Coverage
partialForEnum options =
  merge $ map (uncurry partialForVariant) options

partialForVariant :: String -> [(String, TypeRef)] -> Coverage
partialForVariant enumName fields =
  partialOf enumName $ merge $ map partialForField $ map fst fields

partialForField :: String -> Coverage
partialForField name = partialOf name noCoverage

partialOf :: String -> Coverage -> Coverage
partialOf = Partial . Map.singleton

merge :: [Coverage] -> Coverage
merge = foldl mergeCoverage noCoverage

mergeCoverage :: Coverage -> Coverage -> Coverage
mergeCoverage _ TotalCoverage = TotalCoverage
mergeCoverage TotalCoverage _ = TotalCoverage
mergeCoverage (Partial m1) (Partial m2) =
  Partial $ Map.unionWith mergeCoverage m1 m2
