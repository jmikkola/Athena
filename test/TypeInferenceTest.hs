module Main where

import qualified Data.Map as Map
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), (~=?), assertFailure )
import Test.Framework ( defaultMain )

import TestUtil ( asGroup )

import TypeInference

main = defaultMain $ asGroup [ ("replacements", testReplacements)
                             , ("contains var", testContainsVar)
                             , ("merge equal vars", testMergeEqualVars)
                             , ("running inference", testRunInference)
                             ]

testReplacements = TestList [ testGetMissingVar
                            , testGetExistingVar
                            , testAddNewReplacement
                            , testReplaceExistingVar
                            ]

testGetMissingVar =
  "getting a replacement that doesn't exist returns the original variable" ~:
  (TypeVar "foo") ~=? getReplacement (TypeVar "foo") exampleReplacements

testGetExistingVar =
  "getting a replaced var returns the replacement" ~:
  (TypeVar "b") ~=? getReplacement (TypeVar "f") exampleReplacements

testAddNewReplacement =
  "adding a new replacement" ~:
  addReplacement (TypeVar "a") (TypeVar "b") emptyReplacements ~?=
  createReplacements [("a", "b")]

testReplaceExistingVar =
  "replacing an existing var replaces all references" ~:
  addReplacement (TypeVar "b") (TypeVar "a") exampleReplacements ~?=
  createReplacements [("b", "a"), ("a", "c"), ("f", "a"), ("g", "a")]

testContainsVar = TestList [ testNotContaining
                           , testContaining
                           , testDoesContainSelf
                           , testDoesNotContainSelf
                           ]

testNotContaining =
  "a type that doesn't contain a var" ~:
  False ~=? containsVar exampleKnownTypes (TypeVar "a") (TypeDefinition (TypeName "Foo") [TypeVar "b"])

testContaining =
  "a type that contains a var" ~:
  True ~=? containsVar exampleKnownTypes (TypeVar "c") (TypeDefinition (TypeName "Foo") [TypeVar "a"])

testDoesContainSelf =
  "a type var that contains itself" ~:
  True ~=? containsSelf exampleKnownTypes (TypeVar "loop")

testDoesNotContainSelf =
  "a type var that doesn't contain itself" ~:
  False ~=? containsSelf exampleKnownTypes (TypeVar "a")


testMergeEqualVars = TestList [ testMergeUnknownVars
                              , testMergeOneKnownVar
                              , testMergeOneKnownVar2
                              , testMergeDifferentTypes
                              , testMergeSameTypes
                              ]

testMergeUnknownVars =
  "merge as equal two untyped variables" ~:
  let result = mergeEqual (Map.empty) emptyReplacements (TypeVar "x") (TypeVar "y")
      expected = Right (Map.empty, createReplacements [("x", "y")], [])
  in result ~?= expected

testMergeOneKnownVar =
  "merge as equal a typed variable and an untyped variable" ~:
  let result = mergeEqual exampleKnownTypes emptyReplacements (TypeVar "x") (TypeVar "a")
      expected = Right (exampleKnownTypes, createReplacements [("x", "a")], [])
  in result ~?= expected

testMergeOneKnownVar2 =
  "merge as equal an untyped variable and a typed variable" ~:
  let result = mergeEqual exampleKnownTypes emptyReplacements (TypeVar "a") (TypeVar "x")
      expected = Right (exampleKnownTypes, createReplacements [("x", "a")], [])
  in result ~?= expected

testMergeDifferentTypes =
  "merging incompatible types results in an error" ~:
  True ~=? (isLeft $ mergeEqual exampleKnownTypes emptyReplacements (TypeVar "c") (TypeVar "f"))

testMergeSameTypes =
  "merge two typed variables with the same type" ~:
  let knownTypes = Map.fromList [ (TypeVar "a", TypeDefinition (TypeName "List") [TypeVar "c"])
                                , (TypeVar "b", TypeDefinition (TypeName "List") [TypeVar "c"]) ]
      result = mergeEqual knownTypes emptyReplacements (TypeVar "a") (TypeVar "b")
      knownTypes' = Map.fromList [ (TypeVar "a", TypeDefinition (TypeName "List") [TypeVar "c"]) ]
      replacements' = createReplacements [("b", "a")]
      expected = Right (knownTypes', replacements', [SameType (TypeVar "c") (TypeVar "c")])
  in result ~?= expected

testRunInference = TestList [ testRunSameTypes
                            , testRunRecursiveTypes
                            ]

testRunSameTypes =
  "run inference on three equal type vars" ~:
  let relationships = createRelationships [("a", "b"), ("b", "c")]
      result = runInference relationships emptyKnownTypes emptyReplacements
      expected = Right (emptyKnownTypes, createReplacements [("a", "c"), ("b", "c")])
  in result ~?= expected

testRunRecursiveTypes =
  "run inference in the precense of recursive types" ~:
  let kt = Map.fromList [ (TypeVar "a", TypeDefinition (TypeName "List") [TypeVar "b"])
                        , (TypeVar "x", TypeDefinition (TypeName "List") [TypeVar "y"])
                        , (TypeVar "y", TypeDefinition (TypeName "Int") []) ]
      replacements = emptyReplacements
      relationships = createRelationships [("a", "x")]
      result = runInference relationships kt replacements

      kt' = Map.fromList [ (TypeVar "a", TypeDefinition (TypeName "List") [TypeVar "b"])
                         , (TypeVar "y", TypeDefinition (TypeName "Int") []) ]
      replacements' = createReplacements [("x", "a"), ("b", "y")]
      expected = Right (kt', replacements')
  in result ~?= expected

exampleKnownTypes = Map.fromList [ (TypeVar "a", TypeDefinition (TypeName "List") [TypeVar "b"])
                                 , (TypeVar "b", TypeDefinition (TypeName "Pair") [TypeVar "c", TypeVar "d"])
                                 , (TypeVar "c", TypeDefinition (TypeName "Int") [])
                                 , (TypeVar "f", TypeDefinition (TypeName "Float") [])
                                 , (TypeVar "loop", TypeDefinition (TypeName "Loop") [TypeVar "loop"])
                                 ]

exampleReplacements = createReplacements [("a", "c"), ("f", "b"), ("g", "b")]

createReplacements :: [(String, String)] -> Replacements
createReplacements = Map.fromList . map (\(a,b) -> (TypeVar a, TypeVar b))

createRelationships :: [(String, String)] -> [Relationship]
createRelationships = map createRelationship

createRelationship :: (String, String) -> Relationship
createRelationship (a, b) = SameType (TypeVar a) (TypeVar b)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
