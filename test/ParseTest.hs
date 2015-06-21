module Main where

import Control.Applicative ( (<*) )
import Control.Monad (liftM)

import Test.HUnit ( Test( TestList ), Assertion, (~=?), (@=?), (~:), assertBool)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Text.Parsec ( parse, eof )
import Text.Parsec.String ( Parser )

import Parse


main = defaultMain $ asGroup [ ("digits", testParseDigits)
                             , ("literals", testLiterals)
                             , ("hex literals", testHexLiteral)
                             , ("octal literals", testOctalLiteral)
                             , ("double quoted string", testDoubleQuotedString)
                             ]

asGroup namedTests = map convert namedTests
  where convert (name, fn) = testGroup name (hUnitTestToTests fn)

maybeEqEither :: (Eq a) => Maybe a -> Either l a -> Bool
maybeEqEither (Just a) (Right b) = a == b
maybeEqEither Nothing  (Left _)  = True
maybeEqEither _        _         = False

assertParses :: (Eq a, Show a) => Parser a -> String -> Maybe a -> Assertion
assertParses parser input expectation = assertBool message matched
  where result    = parse (parser <* eof) "test" input
        matched   = maybeEqEither expectation result
        resultStr = case expectation of
          Nothing -> "not parse"
          Just r  -> "parse to " ++ show r
        message   = "expected " ++ input ++ " to " ++ resultStr ++ ", got " ++ (show result)

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

testGoodDigits = tableTest (map (\(t,o) -> (t, Just o)) goodDigitsTestCases) digits

testBadDigits = tableTest (map (\t -> (t, Nothing)) badDigitsTestCases) digits

literalTestCases = [ ("1", Just (LiteralInt 1))
                    , ("2_001_003", Just (LiteralInt 2001003))
                    , ("1.", Just (LiteralFloat 1.0))
                    , ("10_6.2_5", Just (LiteralFloat 106.25))
                    , ("0.125e+2", Just (LiteralFloat 12.5))
                    , ("0x0ff", Just (LiteralInt 255))
                    , ("0o74", Just (LiteralInt 60))
                    , ("1e5", Nothing)
                    , (escapedString "foo bar", Just (LiteralString "foo bar"))
                    ]

testLiterals = tableTest literalTestCases literal

hexTestCases = [ ("0x1", Just 1)
               , ("0xFF", Just 255)
               , ("0xff", Just 255)
               , ("0x400", Just 1024)
               , ("400", Nothing)
               , ("0xG", Nothing)
               ]

testHexLiteral = tableTest hexTestCases hexNum

octalTestCases = [ ("0o1", Just 1)
                 , ("0o20", Just 16)
                 , ("0o77", Just 63)
                 , ("77", Nothing)
                 , ("0o8", Nothing)
                 ]

testOctalLiteral = tableTest octalTestCases octalNum

quoteLiteral = '"' : ""
escapedString s = quoteLiteral ++ s ++ quoteLiteral
doubleQuotedStringCases = [ (escapedString "", Just "")
                          , (escapedString "Hello, world!", Just "Hello, world!")
                          , (escapedString "\\\"", Just "\\\"")
                          , (escapedString "\\n", Just "\\n")
                          ]

testDoubleQuotedString = tableTest doubleQuotedStringCases doubleQuotedString

tableTest cases parseFn = TestList $ map makeTest cases
  where makeTest (input, expectation) = input ~: assertParses parseFn input expectation
