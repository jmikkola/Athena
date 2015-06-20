module ParseTest where

import Data.Monoid ( mempty )
import Test.HUnit
import Test.HUnit.Text ( runTestTT )
import Text.Parsec ( parse )
import Text.Parsec.String ( Parser )
import Test.Framework

main = runTestTT tests

tests = TestList
  [ TestLabel "test1" test1
  ]

maybeEqEither :: (Eq a) => Maybe a -> Either l a -> Bool
maybeEqEither (Just a) (Right b) = a == b
maybeEqEither Nothing  (Left _)  = True
maybeEqEither _        _         = False

{-
assertParses :: (Eq a, Show a) => Parser a -> String -> Maybe a -> Assertion
assertParses parser input expectation = maybeEqEither expectation result
  where result = parse parser "test" input
  -}
test1 = TestCase (assertEqual "test" 1 2)
