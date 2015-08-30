module Main where

import Control.Applicative ( (<*) )
import Control.Monad ( liftM )
import Test.HUnit ( Test( TestList )
                  , Assertion
                  , (~=?)
                  , (@=?)
                  , (~:)
                  , assertBool )
import Test.Framework ( defaultMain
                      , testGroup )
import Test.Framework.Providers.HUnit ( hUnitTestToTests )
import Text.Parsec ( parse, eof )
import Text.Parsec.String ( Parser )

import Parser.BinaryExpr
import TestUtil ( asGroup )

main = defaultMain $ asGroup [ ("expr int", testParseExprInt)
                             , ("expr int (auto)", testParseExprIntAuto)
                             , ("parens", testParenNumbers)
                             , ("factor", testParseFactor)
                             , ("expression", testParseExpression) ]

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
        message   = "expected " ++ show input ++ " to " ++ resultStr ++ ", got " ++ show result

tableTest parseFn cases = TestList $ map makeTest cases
  where makeTest (input, expectation) = input ~: assertParses parseFn input expectation

autoTest parseFn cases = TestList $ map makeTest cases
  where makeTest parsed =
          let unparsed = unparse parsed
          in unparsed ~: (Right parsed) ~=? parse (parseFn <* eof) "test" unparsed

testParseExprInt = tableTest parseExprInt
                   [ ("", Nothing)
                   , (".", Nothing)
                   , ("1", Just $ ExprInt 1)
                   , ("12345", Just $ ExprInt 12345)
                   ]

testParseExprIntAuto =
  autoTest parseExprInt [ ExprInt 1, ExprInt 123, ExprInt 999 ]

testParenNumbers = tableTest parseFactor
                   [ ("1", Just $ ExprInt 1)
                   , ("(1)", Just $ ExprParen $ ExprInt 1)
                   , ("((1))", Just $ ExprParen $ ExprParen $ ExprInt 1)
                   ]

testParseFactor =
  autoTest parseFactor
  [ ExprInt 123
  , ExprParen (ExprInt 123)
  , ExprParen (ExprParen (ExprInt 123))
  ]

-- (+ (* 11 (* 22 (^ 33 44))) (/ 55 (paren (- 77 66))) )

ei = ExprInt
powerExpr = ExprBin (ei 33) Power (ei 44)
timesExpr = ExprBin (ei 22) Times powerExpr
timesExpr2 = ExprBin timesExpr Times (ei 11)
minusExpr = ExprBin (ei 77) Minus (ei 66)
parenExpr = ExprParen minusExpr
divideExpr = ExprBin (ei 55) Divide parenExpr
plusExpr = ExprBin timesExpr2 Plus divideExpr

testParseExpression =
  autoTest parseExpression
  [ ei 123
  , powerExpr
  , timesExpr
  , timesExpr2
  , minusExpr
  , parenExpr
  , divideExpr
  , plusExpr
  ]
