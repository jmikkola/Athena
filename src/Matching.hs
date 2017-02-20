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
checkMatchExpressions exprs =
  let coverageResult = checkExpressions exprs
  in case coverageResult of
      Left err -> Just err
      Right (Partial _) -> Just Incomplete
      Right TotalCoverage -> Nothing

checkExpressions :: [IR.MatchExpression] -> MatchResult Coverage
checkExpressions exprs = do
  _ <- mapM (\e -> evalStateT (duplicatesCheck e) Set.empty) exprs
  foldM addCoverage noCoverage exprs

type VarUseState = StateT (Set String) MatchResult

duplicatesCheck :: IR.MatchExpression -> VarUseState ()
duplicatesCheck matchExpr = case matchExpr of
  MatchAnything -> return ()
  MatchVariable v -> addVar v
  MatchStructure _ fields -> do
    _ <- mapM duplicatesCheck fields
    return ()

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

-- TODO: take a mapping from enum name to options, not just options
addCoverage :: [EnumOption] -> Coverage -> IR.MatchExpression -> MatchResult Coverage
addCoverage _ TotalCoverage expr =
  unreachable $ "unreachable branch: " ++ show expr
addCoverage options partial expr = case expr of
  MatchAnything ->
    return TotalCoverage
  MatchVariable _ ->
    return TotalCoverage
  MatchStructure name fields -> do
    additionalCoverage <- genTree options fields
    let newCoverage = mergeCoverage additionalCoverage partial
    if newCoverage == partial
       then unreachable $ "case does not match anything new: " ++ show expr
       else return newCoverage


genTree :: [EnumOption] -> [IR.MatchExpression] -> MatchResult Coverage
genTree options exprs = undefined

partialForEnum :: [EnumOption] -> Coverage
partialForEnum options =
  merge $ map (uncurry partialForVariant) options

partialForVariant :: String -> [(String, TypeRef)] -> Coverage
partialForVariant enumName fields =
  Partial $ Map.singleton enumName $ merge $ map partialForField $ map fst fields

partialForField :: String -> Coverage
partialForField name = Partial $ Map.singleton name noCoverage

merge :: [Coverage] -> Coverage
merge = foldl mergeCoverage noCoverage

mergeCoverage :: Coverage -> Coverage -> Coverage
mergeCoverage _ TotalCoverage = TotalCoverage
mergeCoverage TotalCoverage _ = TotalCoverage
mergeCoverage (Partial m1) (Partial m2) =
  Partial $ Map.unionWith mergeCoverage m1 m2
