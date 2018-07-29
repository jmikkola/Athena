module Main where

import System.Exit
  ( exitFailure )

import FirstPass
  ( checkReturns )
import qualified AST.Expression as E
import qualified AST.Declaration as D
import qualified AST.Statement as S
import qualified AST.Type as T

main = do
    putStrLn "\n"
    putStrLn "Testing FirstPass"
    runTests tests


tests =
  [ test "empty fn body" testCheckReturns1
  , test "unreachable statement" testCheckReturns2
  ]


testCheckReturns1 :: IO Assertion
testCheckReturns1 = do
  let fn = D.Function () "foo" dummyTypeDecl [] (S.Block () [])
  assertRight $ checkReturns fn

testCheckReturns2 :: IO Assertion
testCheckReturns2 = do
  let returnStmt = S.Return () Nothing
  let stmts = [printStmt, returnStmt, printStmt]
  let fn = D.Function () "foo" dummyTypeDecl [] (S.Block () stmts)
  assertLeft $ checkReturns fn

dummyTypeDecl = T.TypeName "TestType"

printStmt =
  S.Expr () $ E.Call () (E.Var () "print") [E.Val () (E.StrVal () "hello world")]

-----------------------------
-- Simple unit test runner --
-----------------------------

type Test = IO Bool
type Assertion = Either String ()

runTests :: [Test] -> IO ()
runTests ts = do
  (passes, failures) <- getResults ts 0 0
  renderResults passes failures

renderResults :: Int -> Int -> IO ()
renderResults passes failures = do
  let total = passes + failures
  putStrLn $ show passes ++ "/" ++ show total ++ " passed"
  if failures > 0
    then do
    putStrLn $ show failures ++ " failed"
    exitFailure
    else return ()

getResults :: [Test] -> Int -> Int -> IO (Int, Int)
getResults []     passes failures =
  return (passes, failures)
getResults (t:ts) passes failures = do
  result <- t
  if result
     then getResults ts (1 + passes) failures
    else getResults ts passes (1 + failures)


test :: String -> IO Assertion -> Test
test name assertion = do
  result <- assertion
  case result of
   Right _  -> return True
   Left err -> do
     putStrLn $ name ++ " failed: " ++ err
     return False

assertEq :: (Show a, Eq a) => a -> a -> IO Assertion
assertEq x y
  | x == y    = ok
  | otherwise = err $ "not equal: " ++ show x ++ " and " ++ show y

assertLeft :: (Show a) => Either l a -> IO Assertion
assertLeft (Left _)  = ok
assertLeft (Right x) =
  err $ "expected Left, got Right " ++ show x

assertRight :: (Show a) => Either a r -> IO Assertion
assertRight (Right _) = ok
assertRight (Left x)  =
  err $ "expected Right, got Left " ++ show x

err :: String -> IO Assertion
err = return . Left

ok :: IO Assertion
ok = return $ Right ()
