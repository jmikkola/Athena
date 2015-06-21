module Main where

import Test.HUnit ( Test( TestList ), Assertion, (~=?), (@=?), (~:), assertBool)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Text.Parsec ( parse )
import Text.Parsec.String ( Parser )

import Parse


main = defaultMain [digitTests]

digitTests = testGroup "digits" (hUnitTestToTests testParseDigits)


asGroup namedTests = map convert namedTests
  where convert (name, fn) = testGroup name (hUnitTestToTests fn)

maybeEqEither :: (Eq a) => Maybe a -> Either l a -> Bool
maybeEqEither (Just a) (Right b) = a == b
maybeEqEither Nothing  (Left _)  = True
maybeEqEither _        _         = False

assertParses :: (Eq a, Show a) => Parser a -> String -> Maybe a -> Assertion
assertParses parser input expectation = assertBool message matched
  where result    = parse parser "test" input
        matched   = maybeEqEither expectation result
        resultStr = case expectation of
          Nothing -> "not parse"
          Just r  -> "parse to " ++ show r
        message   = "expected " ++ input ++ " to " ++ resultStr

oneEqualsTwo = "one = two" ~: 1 @=? 1

goodDigitsTestCases = [ ("1", "1")
                      , ("10", "10")
                      , ("01", "01")
                      , ("00", "00")
                      , ("123456789", "123456789")
                      , ("1_2_3_4_5_6_7_8_9", "123456789")
                      , ("1_000_000", "1000000")
                      ]

badDigitsTestCases = ["", "_", "1_", "_3", "1__2"]

testParseDigits = TestList [ "Accepts good digits" ~: testGoodDigits
                           , "Rejects bad digits" ~: testBadDigits
                           ]

testGoodDigits = TestList $ map makeTest goodDigitsTestCases
  where makeTest (input, output) = input ~: assertParses digits input (Just output)

testBadDigits = TestList $ map makeTest badDigitsTestCases
  where makeTest input = input ~: assertParses digits input Nothing
