module Main where

import FirstPass
  ( checkReturns )
import qualified AST.Expression as E
import qualified AST.Declaration as D
import qualified AST.Statement as S
import qualified AST.Type as T
import UnitTest
  ( Assertion
  , assertRight
  , assertLeft
  , runTests
  , test )

main = runTests "FirstPass" tests

tests =
  [ test "empty fn body" testCheckReturns1
  , test "unreachable statement" testCheckReturns2
  ]


testCheckReturns1 :: Assertion
testCheckReturns1 = do
  let fn = D.Function () "foo" Nothing [] (S.Block () [])
  assertRight $ checkReturns fn

testCheckReturns2 :: Assertion
testCheckReturns2 = do
  let returnStmt = S.Return () Nothing
  let stmts = [printStmt, returnStmt, printStmt]
  let fn = D.Function () "foo" Nothing [] (S.Block () stmts)
  assertLeft $ checkReturns fn

printStmt =
  S.Expr () $ E.Call () (E.Var () "print") [E.Val () (E.StrVal () "hello world")]
