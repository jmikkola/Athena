module Main where

import qualified Data.Map as Map

import FirstPass
  ( checkReturns
  , makeConstructors
  , Constructor(..) )
import qualified AST.Expression as E
import qualified AST.Declaration as D
import qualified AST.Statement as S
import qualified AST.Type as T
import Types
import UnitTest
  ( Assertion
  , assertEq
  , assertRight
  , assertLeft
  , runTests
  , test )

main = runTests "FirstPass" tests

tests =
  [ test "empty fn body" testCheckReturns1
  , test "unreachable statement" testCheckReturns2
  , test "building struct constructor" testBuildStructConstructor
  ]


testCheckReturns1 :: Assertion
testCheckReturns1 = do
  let fn = D.Function [] "foo" Nothing [] (S.Block [] [])
  assertRight $ checkReturns fn

testCheckReturns2 :: Assertion
testCheckReturns2 = do
  let returnStmt = S.Return [] Nothing
  let stmts = [printStmt, returnStmt, printStmt]
  let fn = D.Function [] "foo" Nothing [] (S.Block [] stmts)
  assertLeft $ checkReturns fn

testBuildStructConstructor :: Assertion
testBuildStructConstructor = do
  let tDef = T.TypeDef { T.defAnn=[], T.defName="Pair", T.defGenerics=["A", "B"] }
  let tDecl = T.Struct [] [("first", T.TypeName [] "A"), ("second", T.TypeName [] "B")]
  let result = makeConstructors [(tDef, tDecl)] Map.empty

  let firstSch = Scheme 2 $ TFunc [TCon "Pair" [TGen 1, TGen 2]] (TGen 1)
  let secondSch = Scheme 2 $ TFunc [TCon "Pair" [TGen 1, TGen 2]] (TGen 2)
  let fields = [("first", firstSch), ("second", secondSch)]
  let sch = Scheme 2 (TFunc [TGen 1, TGen 2] $ TCon "Pair" [TGen 1, TGen 2])
  let ctor = Constructor { ctorFields=fields, ctorType=sch }
  let expected = Map.fromList [("Pair", ctor)]
  assertEq (Right expected) result

printStmt =
  S.Expr [] $ E.Call [] (E.Var [] "print") [E.Val [] (E.StrVal [] "hello world")]
