module Main where

import AST.Annotation (Annotated, getAnnotation)
import qualified AST.Expression as E
import Errors
  ( Error(..) )
import Types
  ( Substitution
  , Type(..)
  , tUnit
  , tInt
  , tBool
  , tString
  , makeSub
  , emptySubstitution )
import Inference
  ( mgu
  , startingEnv
  , runInfer
  , inferExpr )
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
  , test "recursive unification" recursiveUnification
  , test "expression inference" simpleInference ]


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

simpleInference :: Assertion
simpleInference = do
  let intExpr = intVal 123
  assertExprTypes tInt intExpr

  let lessExpr = E.Binary () E.Less (intVal 5) (intVal 6)
  assertExprTypes tBool lessExpr

  let parenExpr = E.Paren () $ strVal "foo"
  assertExprTypes tString parenExpr

  let undefinedVar = E.Var () "bad var"
  assertExprFails undefinedVar

  let badComparison = E.Binary () E.Less (intVal 5) (strVal "bar")
  assertExprFails badComparison


assertExprTypes :: Type -> E.Expression () -> Assertion
assertExprTypes t expr = do
  let result = runInfer $ inferExpr startingEnv expr
  let resultType = fmap getAnnotation result
  let expected = Right (t, ())
  assertEq expected resultType

assertExprFails :: E.Expression () -> Assertion
assertExprFails expr = do
  let result = runInfer $ inferExpr startingEnv expr
  assertLeft result

intVal :: Int -> E.Expression ()
intVal n = E.Val () $ E.IntVal () n

strVal :: String -> E.Expression ()
strVal s = E.Val () $ E.StrVal () s

-- TODO
-- Test inference for simple functions
-- Test finding dependencies (inc. handling shadowing)
-- Test inference for DAGs of functions
-- Test inference for cyclic functions
