module Main where

import Control.Monad (liftM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), assertFailure )
import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import TypeInference

main = defaultMain $
       asGroup [ ("getTypeForVar", testGetTypeForVar)
               , ("getFullTypeForVar", testGetFullTypeForVar) ]

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
