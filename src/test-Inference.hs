module Main where

import AST.Annotation (Annotated, getAnnotation)
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T
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
  , inferExpr
  , inferDecl )
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
  , test "expression inference" simpleInference
  , test "simple functions" functionInference ]


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


functionInference :: Assertion
functionInference = do
  -- f() { return 1; }
  let func1 = func [] [S.Return () $ Just $ intVal 1]
  let type1 = TFunc [] tInt
  assertDeclTypes type1 func1

  -- f(x) { return 1; }
  let func2 = func ["x"] [S.Return () $ Just $ intVal 1]
  let type2 = TFunc [TVar "_v0"] tInt
  assertDeclTypes type2 func2

  -- f(x) { return x; }
  let func3 = func ["x"] [S.Return () $ Just $ E.Var () "x"]
  let type3 = TFunc [TVar "_v0"] (TVar "_v0")
  assertDeclTypes type3 func3


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


assertDeclTypes :: Type -> D.Declaration () -> Assertion
assertDeclTypes t decl = do
  let result = runInfer $ inferDecl startingEnv decl
  let resultType = fmap getAnnotation result
  let expected = Right (t, ())
  assertEq expected resultType


intVal :: Int -> E.Expression ()
intVal n = E.Val () $ E.IntVal () n


strVal :: String -> E.Expression ()
strVal s = E.Val () $ E.StrVal () s


func args stmts =
  let fnname = "<unnamed>"
      fntype = T.TypeName "unused"
      fnbody = S.Block () stmts
  in D.Function () fnname fntype args fnbody


-- TODO
-- Test inference for simple functions
---- f() = 1
---- f(x) = 1
---- f(x) = x
---- f(x) = x + 1
---- f(x) = x > 123
---- f(y) = let a = 1; while a < y { a = a * 2 }; return a
---- f(x, y) = if x > y { return x; } else { return y; }
---- f(x, y) = if x > y { return x; }; return y;
----    should fail:
---- f(x, y) = if x > y { return x; }
-- Test finding dependencies (inc. handling shadowing)
-- Test inference for DAGs of functions
---- let id x = x in (id id) 123
---- let f y = y + 1 in g x = f x
-- Test inference for cyclic functions
