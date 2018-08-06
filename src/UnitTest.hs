module UnitTest where

import System.Exit
  ( exitFailure )


type Test = IO Bool
type Assertion = Either String ()

runTests :: String -> [Test] -> IO ()
runTests name ts = do
  putStrLn $ "\nTesting " ++ name
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


test :: String -> Assertion -> Test
test name assertion = do
  case assertion of
   Right _  -> return True
   Left err -> do
     putStrLn $ name ++ " failed: " ++ err
     return False

assertEq :: (Show a, Eq a) => a -> a -> Assertion
assertEq x y
  | x == y    = ok
  | otherwise = err $ "not equal: " ++ show x ++ " and " ++ show y

assertLeft :: (Show a) => Either l a -> Assertion
assertLeft (Left _)  = ok
assertLeft (Right x) =
  err $ "expected Left, got Right " ++ show x

assertRight :: (Show a) => Either a r -> Assertion
assertRight (Right _) = ok
assertRight (Left x)  =
  err $ "expected Right, got Left " ++ show x

err :: String -> Assertion
err = Left

ok :: Assertion
ok = return ()
