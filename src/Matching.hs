module Matching where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)

import IR
--import Type

data Coverage
  = TotalCoverage
  | Partial (Map (String, String) Coverage)
  deriving (Eq, Show)

data MatchError
  = Unreachable String
  | DuplicateName String
  | Incomplete
  deriving (Eq, Show)

type MatchResult = Either MatchError

checkMatchExpressions :: [IR.MatchExpression] -> Maybe MatchError
checkMatchExpressions exprs =
  let coverageResult = foldM addCoverage noCoverage exprs
  in case coverageResult of
      Left err -> Just err
      Right (Partial _) -> Just Incomplete
      Right TotalCoverage -> Nothing

noCoverage :: Coverage
noCoverage = Partial Map.empty

unreachable :: String -> MatchResult a
unreachable = Left . Unreachable

addCoverage :: Coverage -> IR.MatchExpression -> MatchResult Coverage
addCoverage TotalCoverage expr =
  unreachable $ "unreachable branch: " ++ show expr
addCoverage partial expr = case expr of
  MatchAnything ->
    return TotalCoverage
  MatchVariable _ ->
    return TotalCoverage
  MatchStructure name fields -> do
    additionalCoverage <- genTree name fields
    let newCoverage = mergeCoverage additionalCoverage partial
    if newCoverage == partial
       then unreachable $ "case does not match anything new: " ++ show expr
       else return newCoverage

genTree :: String -> [IR.MatchExpression] -> MatchResult Coverage
genTree = undefined -- TODO: get information about the enum options and the field names of each

mergeCoverage :: Coverage -> Coverage -> Coverage
mergeCoverage _ TotalCoverage = TotalCoverage
mergeCoverage TotalCoverage _ = TotalCoverage
mergeCoverage (Partial m1) (Partial m2) =
  Partial $ Map.unionWith mergeCoverage m1 m2
