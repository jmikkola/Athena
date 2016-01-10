module Main where

import Control.Monad (liftM)
import Data.Either (isLeft)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), assertFailure )
import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import TypeInference

main = defaultMain $
       asGroup [ ("getTypeForVar", testGetTypeForVar)
               , ("getFullTypeForVar", testGetFullTypeForVar)
               , ("equalityPairsFromSet", testEqualityPairsFromSet)
               , ("non-generic inference", testNonGeneric)
               , ("generic inference", testGenericTI)
               ]

emptyResult :: InfResult
emptyResult = (Map.empty, Map.empty)

nilTN = TypeNode "()" []
strTN = TypeNode "String" []
intTN = TypeNode "Int" []
pairTN l r = TypeNode "Pair" [l, r]
listTN l = TypeNode "List" [l]

exampleResult1 :: InfResult
exampleResult1 = (Map.fromList [(1, strTN), (2, intTN),
                                (101, TypeNode "List" [1]), (102, TypeNode "Set" [10])],
                  Map.fromList [(3, 2), (10, 12), (5, 1)])

testGetTypeForVar =
  TestList [ "missing type var" ~: getTypeForVar emptyResult 1 ~?= Nothing
           , "existing type var" ~: getTypeForVar exampleResult1 2 ~?= Just intTN
           , "replaced but missing" ~: getTypeForVar exampleResult1 10 ~?= Nothing
           , "replaced and defined" ~: getTypeForVar exampleResult1 5 ~?= Just strTN ]

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

inferTypeFor :: TypeVar -> [Rules -> Rules] -> ErrorS (Maybe TypeNode)
inferTypeFor var rules = do
  result <- doInfer rules
  return $ getTypeForVar result var

testNonGeneric =
  TestList [ "empty" ~:
             infer emptyRules ~?= Right (Map.empty, Map.empty)

           , "specify" ~:
             infer (specify 1 intTN emptyRules) ~?=
             Right (Map.fromList [(1, intTN)], Map.empty)

           , "equal" ~:
             infer (setEqual 1 2 emptyRules) ~?=
             Right (Map.empty, Map.fromList [(2, 1)])

           , "simple replacement" ~:
             doInfer [setEqual 1 2, specify 1 intTN] ~?=
             Right (Map.fromList [(1, intTN)], Map.fromList [(2, 1)])

           , "replace equal values" ~:
             doInfer [setEqual 1 2, specify 1 intTN, specify 2 intTN] ~?=
             Right (Map.fromList [(1, intTN)], Map.fromList [(2, 1)])

           , "catches incompatible sub" ~:
             isLeft (doInfer [setEqual 1 2, specify 1 intTN, specify 2 strTN]) ~?= True

           , "more complicated sub" ~:
             doInfer [specify 1 intTN, setEqual 3 4, setEqual 1 5, setEqual 1 2,
                      setEqual 5 2, setEqual 4 5] ~?=
             Right (Map.fromList [(1, intTN)], Map.fromList [(2, 1), (3, 1), (4, 1), (5, 1)])

           , "applies recursive equality" ~:
             doInfer [ specify 1 (pairTN 11 12), specify 2 (pairTN 21 22)
                     , specify 11 intTN, specify 22 strTN, setEqual 1 2 ] ~?=
             Right (Map.fromList [(1, (pairTN 11 22)), (11, intTN), (22, strTN)],
                    Map.fromList [(2, 1), (21, 11), (12, 22)])
           ]

testGenericTI =
  TestList ["rejects invalid generic relation" ~:
             isLeft (doInfer [instanceOf 1 2, specify 1 intTN, specify 2 strTN]) ~?= True

           , "applies generic relations" ~:
             inferTypeFor 2 [specify 1 intTN, instanceOf 2 1] ~?=
             Right (Just intTN)

           , "ignores reverse relationships" ~:
             inferTypeFor 2 [specify 1 intTN, instanceOf 1 2] ~?=
             Right Nothing

           , "accepts circular relationships" ~:
             inferTypeFor 2 [specify 1 intTN, instanceOf 2 1, instanceOf 1 2] ~?=
             Right (Just intTN)

           , "applies generics recursively" ~:
             doInfer [ specify 1 (pairTN 11 12), specify 2 (pairTN 21 22)
                     , specify 11 intTN, specify 22 strTN, instanceOf 1 2 ] ~?=
             Right (Map.fromList [ (1, pairTN 11 12), (2, pairTN 21 22)
                                 , (11, intTN), (12, strTN), (22, strTN) ],
                    Map.empty)

           , "allows multiple generic instantiations" ~:
             doInfer [ specify 1 (listTN 11), specify 2 (listTN 21), specify 3 (listTN 31)
                     , specify 21 intTN, specify 31 strTN
                     , instanceOf 2 1, instanceOf 3 1 ] ~?=
             Right (Map.fromList [ (1, listTN 11), (2, listTN 21), (3, listTN 31)
                                 , (21, intTN), (31, strTN) ],
                    Map.empty)

           , "applies equality constraint from generic" ~:
             inferTypeFor 22 [ specify 1 (TypeNode "Fn2" [11, 11, 12])
                             , specify 2 (TypeNode "Fn2" [21, 22, 23])
                             , specify 12 nilTN, specify 21 intTN
                             , instanceOf 2 1] ~?=
             Right (Just intTN)

           , "applies generics for multiple levels" ~:
             doInfer [specify 1 (listTN 11), specify 11 intTN, instanceOf 2 1, instanceOf 3 2] ~?=
             -- TODO: is this actually right? or should 2 and 3 have generic instances of 11?
             Right (Map.fromList [(1, listTN 11), (2, listTN 11), (3, listTN 11), (11, intTN)],
                    Map.empty)

           , "catches generic errors with separation" ~:
             isLeft (doInfer [ specify 1 (listTN 11), specify 11 intTN
                             , specify 4 (listTN 41), specify 41 strTN
                             , instanceOf 2 1, instanceOf 4 3, instanceOf 3 2 ]) ~?=
             True

           , "merges subcomponents" ~:
             doInfer [ specify 1 intTN, instanceOf 2 1, instanceOf 1 2, instanceOf 3 2]
             ~?= Right (Map.fromList [(1, intTN), (3, intTN)], Map.fromList [(2, 1)])
           ]

emptyVarSet :: Set TypeVar
emptyVarSet = Set.empty

testEqualityPairsFromSet =
  TestList [ "empty" ~: equalityPairsFromSet emptyVarSet ~?= []
           , "1 elem" ~: equalityPairsFromSet (Set.singleton 1) ~?= []
           , "multiple elems" ~: equalityPairsFromSet (Set.fromList [1, 2, 3, 4]) ~?=
             [(2, 1), (3, 1), (4, 1)]
           ]
