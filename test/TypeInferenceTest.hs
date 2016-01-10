module Main where

import Control.Monad (liftM)
import Data.Either (isLeft)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), assertFailure )
import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import TypeInference

main = defaultMain $
       asGroup [ ("getTypeForVar", testGetTypeForVar)
               , ("getFullTypeForVar", testGetFullTypeForVar)
               , ("non-generic inference", testNonGeneric) ]

emptyResult :: InfResult
emptyResult = (Map.empty, Map.empty)

strTypeNode = TypeNode "String" []
intTypeNode = TypeNode "Int" []

exampleResult1 :: InfResult
exampleResult1 = (Map.fromList [(1, strTypeNode), (2, intTypeNode),
                                (101, TypeNode "List" [1]), (102, TypeNode "Set" [10])],
                  Map.fromList [(3, 2), (10, 12), (5, 1)])

testGetTypeForVar =
  TestList [ "missing type var" ~: getTypeForVar emptyResult 1 ~?= Nothing
           , "existing type var" ~: getTypeForVar exampleResult1 2 ~?= Just intTypeNode
           , "replaced but missing" ~: getTypeForVar exampleResult1 10 ~?= Nothing
           , "replaced and defined" ~: getTypeForVar exampleResult1 5 ~?= Just strTypeNode ]

testGetFullTypeForVar =
  TestList [ "simple type" ~: getFullTypeForVar exampleResult1 1 ~?=
             (Just $ Constructor "String" [])
           , "complex with var" ~: getFullTypeForVar exampleResult1 102 ~?=
             (Just $ Constructor "Set" [Var 10])
           , "complex with inner"~: getFullTypeForVar exampleResult1 101 ~?=
             (Just $ Constructor "List" [Constructor "String" []]) ]

doInfer :: [Rules -> Rules] -> ErrorS InfResult
doInfer = infer . makeRules

makeRules :: [Rules -> Rules] -> Rules
makeRules [] = emptyRules
makeRules (r:rs) = r (makeRules rs)

testNonGeneric =
  TestList [ "empty" ~: infer emptyRules ~?= Right (Map.empty, Map.empty)
           , "specify" ~: infer (specify 1 intTypeNode emptyRules) ~?=
             Right (Map.fromList [(1, intTypeNode)], Map.empty)
           , "equal" ~: infer (setEqual 1 2 emptyRules) ~?=
             Right (Map.empty, Map.fromList [(2, 1)])
           , "simple replacement" ~:
             doInfer [setEqual 1 2, specify 1 intTypeNode] ~?=
             Right (Map.fromList [(1, intTypeNode)], Map.fromList [(2, 1)])
           , "replace equal values" ~:
             doInfer [setEqual 1 2, specify 1 intTypeNode, specify 2 intTypeNode] ~?=
             Right (Map.fromList [(1, intTypeNode)], Map.fromList [(2, 1)])
           , "catches incompatible sub" ~:
             isLeft (doInfer [setEqual 1 2, specify 1 intTypeNode, specify 2 strTypeNode])
             ~?= True
           , "more complicated sub" ~:
             doInfer [specify 1 intTypeNode, setEqual 3 4, setEqual 1 5, setEqual 1 2,
                      setEqual 5 2, setEqual 4 5] ~?=
             Right (Map.fromList [(1, intTypeNode)], Map.fromList [(2, 1), (3, 1), (4, 1), (5, 1)])
           ]
