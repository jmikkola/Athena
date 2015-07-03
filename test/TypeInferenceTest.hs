module Main where

import qualified Data.Map as Map
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), (~=?), assertFailure )
import Test.Framework ( defaultMain )

import TestUtil ( asGroup )

import TypeInference

main = defaultMain $ asGroup [ ("replacements", testReplacements)
                             ]

testReplacements = TestList [ testGetMissingVar
                            , testGetExistingVar
                            , testAddNewReplacement
                            , testReplaceExistingVar
                            ]

testGetMissingVar = "getting a replacement that doesn't exist returns the original variable" ~:
                    (TypeVar "foo") ~=? getReplacement (TypeVar "foo") exampleReplacements

testGetExistingVar = "getting a replaced var returns the replacement" ~:
                     (TypeVar "b") ~=? getReplacement (TypeVar "f") exampleReplacements

testAddNewReplacement = "adding a new replacement" ~:
                        addReplacement (TypeVar "a") (TypeVar "b") emptyReplacements ~?=
                        createReplacements [("a", "b")]

testReplaceExistingVar = "replacing an existing var replaces all references" ~:
                         addReplacement (TypeVar "b") (TypeVar "a") exampleReplacements ~?=
                         createReplacements [("b", "a"), ("a", "c"), ("f", "a"), ("g", "a")]

exampleReplacements = createReplacements [("a", "c"), ("f", "b"), ("g", "b")]

createReplacements :: [(String, String)] -> Replacements
createReplacements = Map.fromList . map (\(a,b) -> (TypeVar a, TypeVar b))
