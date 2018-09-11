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
  , tUnit
  , makeSub
  , emptySubstitution )
import Inference
  ( mgu
  , startingEnv
  , runInfer
  , inferExpr
  , inferDecl
  , unifies )
import UnitTest
  ( Assertion
  , Test
  , assert
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
  , test "simple functions" functionInference
  , test "while loop" whileLoop
  , test "if-else return" ifElseReturn
  , test "if-then return" ifThenReturn
  , test "return a-b-c" returnABC
  , test "missing return" missingReturn ]


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
  -- f() { }
  let func0 = func [] []
  let type0 = TFunc [] tUnit
  assertDeclTypes type0 func0

  -- f() { return 1; }
  let func1 = func [] [returnJust $ intVal 1]
  let type1 = TFunc [] tInt
  assertDeclTypes type1 func1

  -- f(x) { return 1; }
  let func2 = func ["x"] [returnJust $ intVal 1]
  let type2 = TFunc [TVar "a"] tInt
  assertDeclTypes type2 func2

  -- f(x) { return x; }
  let func3 = func ["x"] [returnJust $ E.Var () "x"]
  let type3 = TFunc [TVar "a"] (TVar "a")
  assertDeclTypes type3 func3

  -- f(x) { return x + 1; }
  let func4 = func ["x"] [returnJust $ E.Binary () E.Plus (E.Var () "x") (intVal 1)]
  let type4 = TFunc [tInt] tInt
  assertDeclTypes type4 func4

  -- f(x) { return x > 123; }
  let func5 = func ["x"] [returnJust $ E.Binary () E.Less (E.Var () "x") (intVal 123)]
  let type5 = TFunc [tInt] tBool
  assertDeclTypes type5 func5


whileLoop :: Assertion
whileLoop = do
  -- f(y) = let a = 1; while a < y { a = a * 2 }; return a
  let aTo1 = S.Let () "a" (intVal 1)
  let aLessY = E.Binary () E.Less (E.Var () "a") (E.Var () "y")
  let aTimes2 = E.Binary () E.Times (E.Var () "a") (intVal 2)
  let whileBody = S.Assign () ["a"] aTimes2
  let while = S.While () aLessY [whileBody]
  let returnA = returnJust $ E.Var () "a"
  let func6 = func ["y"] [aTo1, while, returnA]
  let type6 = TFunc [tInt] tInt
  assertDeclTypes type6 func6


ifElseReturn :: Assertion
ifElseReturn = do
  -- f(x, y) = if x > y { return x; } else { return y; }
  let test = E.Binary () E.Greater (E.Var () "x") (E.Var () "y")
  let returnX = returnJust $ E.Var () "x"
  let returnY = returnJust $ E.Var () "y"
  let ifStmt = S.If () test [returnX] (Just returnY)
  let func7 = func ["x", "y"] [ifStmt]
  let type7 = TFunc [tInt, tInt] tInt
  assertDeclTypes type7 func7


ifThenReturn :: Assertion
ifThenReturn = do
  -- f(x, y) = if x > y { return x; }; return y;
  let test = E.Binary () E.Greater (E.Var () "x") (E.Var () "y")
  let returnX = returnJust $ E.Var () "x"
  let ifStmt = S.If () test [returnX] Nothing
  let returnY = returnJust $ E.Var () "y"
  let func8 = func ["x", "y"] [ifStmt, returnY]
  let type8 = TFunc [tInt, tInt] tInt
  assertDeclTypes type8 func8


returnABC :: Assertion
returnABC = do
  -- f(a, b, c) = if a { return b; } else { return c; }
  let returnB = returnJust $ E.Var () "b"
  let returnC = returnJust $ E.Var () "c"
  let ifStmt = S.If () (E.Var () "a") [returnB] (Just returnC)
  let func9 = func ["a", "b", "c"] [ifStmt]
  let type9 = TFunc [tBool, TVar "a", TVar "a"] (TVar "a")
  assertDeclTypes type9 func9


missingReturn :: Assertion
missingReturn = do
  -- f(x, y) = if x > y { return x; }
  let returnX = returnJust $ E.Var () "x"
  let test = E.Binary () E.Greater (E.Var () "x") (E.Var () "y")
  let ifStmt = S.If () test [returnX] Nothing
  assertDeclFails $ func ["x", "y"] [ifStmt]


returnJust expr = S.Return () (Just expr)


assertExprTypes :: Type -> E.Expression () -> Assertion
assertExprTypes t expr = assertTypes t expr inferExpr


assertExprFails :: E.Expression () -> Assertion
assertExprFails expr = assertFails expr inferExpr


assertDeclTypes :: Type -> D.Declaration () -> Assertion
assertDeclTypes t decl = assertTypes t decl inferDecl


assertDeclFails :: D.Declaration () -> Assertion
assertDeclFails decl = assertFails decl inferDecl


assertTypes t ast inferFn = do
  let result = runInfer $ inferFn startingEnv ast
  assertRight result
  let (Right typed) = result
  let resultType = fst . getAnnotation $ typed
  assertUnifies t resultType


assertFails ast inferFn = do
  let result = runInfer $ inferFn startingEnv ast
  assertLeft result


assertUnifies :: Type -> Type -> Assertion
assertUnifies expected result = do
  let message = "expected " ++ show result ++ " to unify with " ++ show expected
  assert (unifies expected result) message


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
-- Test finding dependencies (inc. handling shadowing)
-- Test inference for DAGs of functions
---- let id x = x in (id id) 123
---- let f y = y + 1 in g x = f x
-- Test inference for cyclic functions
