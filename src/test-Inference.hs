module Main where

import Errors
  ( Error(..) )
import Types
  ( Substitution
  , Type(..)
  , tUnit
  , tInt
  , makeSub
  , emptySubstitution )
import Inference
  ( mgu )
import UnitTest
  ( Assertion
  , Test
  , assertEq
  , assertRight
  , assertLeft
  , runTests
  , test )

main = runTests "Inference" tests

tests :: [Test]
tests =
  [ test "basic unification" basicUnification
  , test "recursive unification" recursiveUnification ]


basicUnification :: Assertion
basicUnification = do
  let result1 = mgu tUnit tUnit
  assertEq (Right emptySubstitution) result1

  let result2 = mgu tUnit tInt
  assertLeft result2

  let result3 = mgu (TVar "a") tInt
  assertEq (Right $ makeSub [(TVar "a", tInt)]) result3

  let result4 = mgu (TVar "a") (TVar "a")
  assertEq (Right emptySubstitution) result4

  let result5 = mgu tInt (TVar "x")
  assertEq (Right $ makeSub [(TVar "x", tInt)]) result5

  let result6 = mgu (TVar "a") (TVar "b")
  assertEq (Right $ makeSub [(TVar "a", TVar "b")]) result6

  let result7 = mgu (TVar "a") (TFunc [TVar "a"] tInt)
  assertEq (Left $ InfiniteType "a") result7

recursiveUnification :: Assertion
recursiveUnification = do
  let result1 = mgu (TFunc [TVar "a"] (TVar "a")) (TFunc [TVar "b"] tInt)
  let expected1 = makeSub [(TVar "a", tInt), (TVar "b", tInt)]
  assertEq (Right expected1) result1

  let result2 = mgu (TFunc [TVar "a"] (TVar "b")) (TFunc [TVar "b"] (TVar "a"))
  let expected2 = makeSub [(TVar "a", TVar "b")]
  assertEq (Right expected2) result2

  let result3 = mgu (TFunc [tInt] (TVar "a")) (TFunc [TVar "a"] tUnit)
  assertLeft result3
