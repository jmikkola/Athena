module Main where

import qualified Data.Map as Map
import Debug.Trace (trace)

import AST.Annotation (Annotated, getAnnotation)
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T

import FirstPass
  ( Module(..) )

import Types
  ( Substitution
  , Type(..)
  , Scheme(..)
  , apply
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
  , unifies
  , makeBindGroups
  , implicitBindings
  , inferModule
  , instantiate
  , InferResult(..)
  , BindGroup(..) )

import Errors
  ( Error(..)
  , Result )

import UnitTest
  ( Assertion
  , Test
  , err
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
  , test "instantiation" instantiation
  , test "expression inference" simpleInference
  , test "simple functions" functionInference
  , test "while loop" whileLoop
  , test "if-else return" ifElseReturn
  , test "if-then return" ifThenReturn
  , test "return a-b-c" returnABC
  , test "missing return" missingReturn
  , test "first class function" firstClassFunction
  , test "no higher order polymorphism" noHigherOrderPolymorphism
  , test "infinite type" infiniteType
  , test "finding dependencies" findDependencies
  , test "simple module" simpleModule ]


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


instantiation :: Assertion
instantiation = do
  assertInstantiates (Scheme 0 tInt) tInt
  assertInstantiates (Scheme 1 tInt) tInt
  assertInstantiates (Scheme 1 $ TGen 1) (TVar "a")
  let sch2 = Scheme 2 (TFunc [TGen 1, TGen 2] (TGen 2))
  let t2 = TFunc [TVar "a", TVar "b"] (TVar "b")
  assertInstantiates sch2 t2


simpleInference :: Assertion
simpleInference = do
  let intExpr = intVal 123
  assertExprTypes tInt intExpr

  let lessExpr = E.Binary () E.Less (intVal 5) (intVal 6)
  assertExprTypes tBool lessExpr

  let notExpr = E.Unary () E.BoolNot (boolVal True)
  assertExprTypes tBool notExpr

  let parenExpr = E.Paren () $ strVal "foo"
  assertExprTypes tString parenExpr

  let undefinedVar = E.Var () "bad var"
  assertExprFails undefinedVar

  let badComparison = E.Binary () E.Less (intVal 5) (strVal "bar")
  assertExprFails badComparison

  -- 3()
  let badCall = E.Call () (intVal 3) []
  assertExprFails badCall

  -- 3(5)
  let badCall2 = E.Call () (intVal 3) [intVal 5]
  assertExprFails badCall2


functionInference :: Assertion
functionInference = do
  -- shared definitions
  let varX = E.Var () "x"

  -- f() { }
  let func0 = func "f" [] []
  let type0 = TFunc [] tUnit
  assertDeclTypes type0 func0

  -- f() { return 1; }
  let func1 = func "f" [] [returnJust $ intVal 1]
  let type1 = TFunc [] tInt
  assertDeclTypes type1 func1

  -- f(x) { return 1; }
  let func2 = func "f" ["x"] [returnJust $ intVal 1]
  let type2 = TFunc [TVar "a"] tInt
  assertDeclTypes type2 func2

  -- f(x) { return x; }
  let func3 = func "f" ["x"] [returnJust varX]
  let type3 = TFunc [TVar "a"] (TVar "a")
  assertDeclTypes type3 func3

  -- f(x) { return x + 1; }
  let func4 = func "f" ["x"] [returnJust $ E.Binary () E.Plus varX (intVal 1)]
  let type4 = TFunc [tInt] tInt
  assertDeclTypes type4 func4

  -- f(x) { return x > 123; }
  let func5 = func "f" ["x"] [returnJust $ E.Binary () E.Less varX (intVal 123)]
  let type5 = TFunc [tInt] tBool
  assertDeclTypes type5 func5

  -- f(x) { return x && True; }
  let funcBool = func "f" ["x"] [returnJust $ E.Binary () E.BoolAnd varX (boolVal True)]
  let typeBool = TFunc [tBool] tBool
  assertDeclTypes typeBool funcBool


whileLoop :: Assertion
whileLoop = do
  -- f(y) = let a = 1; while a < y { a = a * 2 }; return a
  let aTo1 = S.Let () "a" Nothing (intVal 1)
  let aLessY = E.Binary () E.Less (E.Var () "a") (E.Var () "y")
  let aTimes2 = E.Binary () E.Times (E.Var () "a") (intVal 2)
  let whileBody = S.Assign () ["a"] aTimes2
  let while = S.While () aLessY [whileBody]
  let returnA = returnJust $ E.Var () "a"
  let func6 = func "f" ["y"] [aTo1, while, returnA]
  let type6 = TFunc [tInt] tInt
  assertDeclTypes type6 func6


ifElseReturn :: Assertion
ifElseReturn = do
  -- f(x, y) = if x > y { return x; } else { return y; }
  let test = E.Binary () E.Greater (E.Var () "x") (E.Var () "y")
  let returnX = returnJust $ E.Var () "x"
  let returnY = returnJust $ E.Var () "y"
  let ifStmt = S.If () test [returnX] (Just returnY)
  let func7 = func "f" ["x", "y"] [ifStmt]
  let type7 = TFunc [tInt, tInt] tInt
  assertDeclTypes type7 func7


ifThenReturn :: Assertion
ifThenReturn = do
  -- f(x, y) = if x > y { return x; }; return y;
  let test = E.Binary () E.Greater (E.Var () "x") (E.Var () "y")
  let returnX = returnJust $ E.Var () "x"
  let ifStmt = S.If () test [returnX] Nothing
  let returnY = returnJust $ E.Var () "y"
  let func8 = func "f" ["x", "y"] [ifStmt, returnY]
  let type8 = TFunc [tInt, tInt] tInt
  assertDeclTypes type8 func8


returnABC :: Assertion
returnABC = do
  -- f(a, b, c) = if a { return b; } else { return c; }
  let returnB = returnJust $ E.Var () "b"
  let returnC = returnJust $ E.Var () "c"
  let ifStmt = S.If () (E.Var () "a") [returnB] (Just returnC)
  let func9 = func "f" ["a", "b", "c"] [ifStmt]
  let type9 = TFunc [tBool, TVar "a", TVar "a"] (TVar "a")
  assertDeclTypes type9 func9


missingReturn :: Assertion
missingReturn = do
  -- f(x, y) = if x > y { return x; }
  let returnX = returnJust $ E.Var () "x"
  let test = E.Binary () E.Greater (E.Var () "x") (E.Var () "y")
  let ifStmt = S.If () test [returnX] Nothing
  assertDeclFails $ func "f" ["x", "y"] [ifStmt]


firstClassFunction :: Assertion
firstClassFunction = do
  -- f(x, y) = { return x(y, y); }
  let varX = E.Var () "x"
  let varY = E.Var () "y"
  let call = E.Call () varX [varY, varY]
  let f = func "f" ["x", "y"] [returnJust call]
  -- (a -> a -> b)
  let xType = TFunc [TVar "a", TVar "a"] (TVar "b")
  -- (a -> a -> b) -> a -> b
  let t = TFunc [xType, TVar "a"] (TVar "b")
  assertDeclTypes t f


noHigherOrderPolymorphism :: Assertion
noHigherOrderPolymorphism = do
  -- f(x) { return x(x(1) > 2); }
  let varX = E.Var () "x"
  let innerCall = E.Call () varX [intVal 1]
  let comparison = E.Binary () E.Greater innerCall (intVal 2)
  let call = E.Call () varX [comparison]
  let f = func "f" ["x"] [returnJust call]
  assertDeclFails f


infiniteType :: Assertion
infiniteType = do
  -- f(x) { return x(x); }
  let varX = E.Var () "x"
  let call = E.Call () varX [varX]
  let f = func "f" ["x"] [returnJust call]
  assertDeclFails f


findDependencies :: Assertion
findDependencies = do
  let varX = E.Var () "x"
  let varF = E.Var () "f"
  let varG = E.Var () "g"

  -- f(x) { return g(x); }
  -- g(x) { return x; }
  let fCallsG = func "f" ["x"] [returnJust $ E.Call () varG [varX]]
  let g = func "g" ["x"] [returnJust varX]
  assertEq [["g"], ["f"]] (findGroups [("f", fCallsG), ("g", g)])

  -- f(x) { return g(x); }
  -- g(x) { return f(x); }
  -- h() { return g; }
  let gCallsF = func "g" ["x"] [returnJust $ E.Call () varF [varX]]
  let hReturnsG = func "h" [] [returnJust varG]
  let bindings2 = [("f", fCallsG), ("g", gCallsF), ("h", hReturnsG)]
  assertEq [["g", "f"], ["h"]] (findGroups bindings2)

  -- TODO: Test more deeply nested AST
  -- TODO: Test larger call graphs w/ longer cycles
  -- TODO: Test shadowing via arg names
  -- TODO: Test shadowing via let statements


simpleModule :: Assertion
simpleModule = do
  let varF = E.Var () "f"
  let varN = E.Var () "n"
  let varX = E.Var () "x"
  let varID = E.Var () "id"

  -- Test a super basic module
  -- f(n) { return n + 1; }
  let nPlus1 = func "f" ["n"] [returnJust $ E.Binary () E.Plus varN (intVal 1)]
  let result1 = inferModule $ makeModule [("f", nPlus1)]
  let intFn = Scheme 0 $ TFunc [tInt] tInt
  assertModuleTypes "f" intFn result1

  -- Test basic let-polymorphism
  -- id(x) { return x; }
  let identity = func "id" ["x"] [returnJust varX]
  let result2 = inferModule $ makeModule [("id", identity)]
  let idType = Scheme 1 $ TFunc [TGen 1] (TGen 1)
  assertModuleTypes "id" idType result2

  -- Test usage of let-polymorphism
  -- id(x) { return x; }
  -- f(n) { return id(n > id(3)); }
  let id3 = E.Call () varID [intVal 3]
  let idExpr = E.Call () varID [E.Binary () E.Greater varN id3]
  let fN = func "f" ["n"] [returnJust idExpr]
  let result3 = inferModule $ makeModule [("f", fN), ("id", identity)]
  let fNType = Scheme 0 $ TFunc [tInt] tBool
  assertModuleTypes "f" fNType result3
  assertModuleTypes "id" idType result3

  -- Test the fact that mutually-recursive functions
  -- are sometimes less general than you'd expect
  -- id(x) { f(1); return x; }
  -- f(x) { return id(x) > 2; }
  let callF = S.Expr () $ E.Call () varF [intVal 1]
  let identityCallingF = func "id" ["x"] [callF, returnJust varX]
  let idOfX = E.Call () varID [varX]
  let fCallsID = func "f" ["x"] [returnJust $ E.Binary () E.Greater idOfX (intVal 2)]
  let result4 = inferModule $ makeModule [("f", fCallsID), ("id", identityCallingF)]
  let lessGeneralIDType = Scheme 1 $ TFunc [tInt] tInt
  let fCallsIDType = Scheme 1 $ TFunc [tInt] tBool
  assertModuleTypes "f" fCallsIDType result4
  assertModuleTypes "id" lessGeneralIDType result4


assertModuleTypes :: String -> Scheme -> Result (InferResult a) -> Assertion
assertModuleTypes name sch result = case result of
  Left msg          -> err $ "failed to infer type for module: " ++ show msg
  Right inferResult ->
    case Map.lookup name $ topLevelEnv inferResult of
     Nothing        -> err $ "can't find " ++ name
     Just resultSch -> assertSchemeUnifies sch resultSch


makeModule :: [(String, D.Declaration a)] -> Module a
makeModule bindings =
  let bindMap = Map.fromList bindings
  in Module { bindings=bindMap, types=Map.empty }


findGroups :: [(String, D.Declaration a)] -> [[String]]
findGroups bindings =
  getGroupNames $ makeBindGroups $ makeModule bindings


getGroupNames :: [BindGroup a] -> [[String]]
getGroupNames = map getNames
  where getNames bg = map fst $ implicitBindings bg


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
  let result = runInfer Map.empty $ inferFn startingEnv ast
  assertRight result
  let (Right typed) = result
  let resultType = fst . getAnnotation $ typed
  assertUnifies t resultType


assertFails ast inferFn = do
  let result = runInfer Map.empty $ inferFn startingEnv ast
  assertLeft result


-- TODO: This should really be "assert matches" not "assert unifies"
-- so that it doesn't allow narrower types than it should.
assertUnifies :: Type -> Type -> Assertion
assertUnifies expected result = do
  assertNoGenerics expected
  assertNoGenerics result
  let message = "expected " ++ show result ++ " to unify with " ++ show expected
  assert (unifies expected result) message


assertInstantiates :: Scheme -> Type -> Assertion
assertInstantiates sch = assertUnifies (runInstantiate sch)


runInstantiate :: Scheme -> Type
runInstantiate sch =
  let (Right result) = runInfer Map.empty (instantiate sch)
  in result


assertNoGenerics :: Type -> Assertion
assertNoGenerics t =
  let message = "expected the type (" ++ show t ++ ") to not contain generics"
  in assert (not $ containsGenerics t) message


containsGenerics :: Type -> Bool
containsGenerics t = case t of
  TCon _ ts  -> any containsGenerics ts
  TFunc as t -> any containsGenerics as || containsGenerics t
  TVar _     -> False
  TGen _     -> True


-- expected, result
assertSchemeUnifies :: Scheme -> Scheme -> Assertion
assertSchemeUnifies s1@(Scheme n1 _) s2@(Scheme n2 _) = do
  assertUnifies (testInstantiate s1) (testInstantiate s2)
  assertEq n1 n2


-- testInstantiate instantiates without the InferM monad available
testInstantiate :: Scheme -> Type
testInstantiate (Scheme n t) =
  let range = [1..n]
      newVars = [TVar $ "-t" ++ show i | i <- range]
      genVars = map TGen range
      sub = Map.fromList $ zip genVars newVars
  in apply sub t


intVal :: Int -> E.Expression ()
intVal n = E.Val () $ E.IntVal () n


strVal :: String -> E.Expression ()
strVal s = E.Val () $ E.StrVal () s


boolVal :: Bool -> E.Expression ()
boolVal b = E.Val () $ E.BoolVal () b


func :: String -> [String] -> [S.Statement ()] -> D.Declaration ()
func name args stmts =
  let fnbody = S.Block () stmts
  in D.Function () name Nothing args fnbody


-- TODO
-- Test inference for DAGs of functions
---- let id x = x in (id id) 123
---- let f y = y + 1 in g x = f x
-- Test inference for cyclic functions
-- Test all pairwise combinations of syntax (e.g. trying to call 3 as a function)
