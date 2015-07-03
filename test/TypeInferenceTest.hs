module Main where

import qualified Data.Map as Map
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), (~=?), assertFailure )
import Test.Framework ( defaultMain )

import TestUtil ( asGroup )

import TypeInference

main = defaultMain $ asGroup [ ("replacements", testReplacements)
                             , ("contains var", testContainsVar)
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
                           , testSimpleContaining
                           ]

testNotContaining =
  "a type that doesn't contain a var" ~:
  False ~=? containsVar exampleKnownTypes (TypeVar "a") (TypeDefinition (TypeName "Foo") [TypeVar "b"])

testSimpleContaining =
  "a type that clearly contains a var" ~:
  True ~=? containsVar exampleKnownTypes (TypeVar "c") (TypeDefinition (TypeName "Foo") [TypeVar "a"])

exampleKnownTypes = Map.fromList [ (TypeVar "a", TypeDefinition (TypeName "List") [TypeVar "b"])
                                 , (TypeVar "b", TypeDefinition (TypeName "Pair") [TypeVar "c", TypeVar "d"])
                                 , (TypeVar "c", TypeDefinition (TypeName "Int") [])
                                 , (TypeVar "f", TypeDefinition (TypeName "Float") [])
                                 ]

exampleReplacements = createReplacements [("a", "c"), ("f", "b"), ("g", "b")]

createReplacements :: [(String, String)] -> Replacements
createReplacements = Map.fromList . map (\(a,b) -> (TypeVar a, TypeVar b))
