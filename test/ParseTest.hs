module Main where

import Control.Applicative ( (<*) )
import Control.Monad (liftM)

import Test.HUnit ( Test( TestList ), Assertion, (~=?), (@=?), (~:), assertBool)
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

import Text.Parsec ( parse, eof )
import Text.Parsec.String ( Parser )

import TestUtil ( asGroup )

import Parse

main = defaultMain $ asGroup [ ("digits", testParseDigits)
                             , ("literals", testLiterals)
                             , ("hex literals", testHexLiteral)
                             , ("octal literals", testOctalLiteral)
                             , ("double quoted string", testDoubleQuotedString)
                             , ("function call", testFnCall)
                             , ("expression", testExpression)
                             , ("fn call args", testFnArgs)
                             , ("binary expression", testBinaryExpr)
                             , ("statements", testStatements)
                             , ("block", testBlock)
                             , ("type def", testTypeDef)
                             , ("function", testFunctionDef)
                             ]

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

tableTest parseFn cases = TestList $ map makeTest cases
  where makeTest (input, expectation) = input ~: assertParses parseFn input expectation

expectNoParse s =  (s, Nothing)
expectParsesTo (s,o) = (s, Just o)

testParseDigits = TestList [ "Accepts good digits" ~: testGoodDigits
                           , "Rejects bad digits" ~: testBadDigits
                           ]

testGoodDigits = tableTest digits
                 (map expectParsesTo [ ("1", "1")
                                     , ("10", "10")
                                     , ("01", "01")
                                     , ("00", "00")
                                     , ("123456789", "123456789")
                                     , ("1_2_3_4_5_6_7_8_9", "123456789")
                                     , ("1_000_000", "1000000")
                                     ])

testBadDigits = tableTest digits (map expectNoParse ["", "_", "1_", "_3", "1__2"])

testLiterals = tableTest literal
               [ ("1", Just (LiteralInt 1))
               , ("2_001_003", Just (LiteralInt 2001003))
               , ("1.", Just (LiteralFloat 1.0))
               , ("10_6.2_5", Just (LiteralFloat 106.25))
               , ("0.125e+2", Just (LiteralFloat 12.5))
               , ("0x0ff", Just (LiteralInt 255))
               , ("0o74", Just (LiteralInt 60))
               , ("1e5", Nothing)
               , (escapedString "foo bar", Just (LiteralString "foo bar"))
               , ("False", Just (LiteralStruct "False"))
               ]

testHexLiteral = tableTest hexNum
                 [ ("0x1", Just 1)
                 , ("0xFF", Just 255)
                 , ("0xff", Just 255)
                 , ("0x400", Just 1024)
                 , ("400", Nothing)
                 , ("0xG", Nothing)
                 ]

testOctalLiteral = tableTest octalNum
                   [ ("0o1", Just 1)
                   , ("0o20", Just 16)
                   , ("0o77", Just 63)
                   , ("77", Nothing)
                   , ("0o8", Nothing)
                   ]

quoteCh = '"'
backslashCr = '\\'
escapedString s = quoteCh : s ++ [quoteCh]

testDoubleQuotedString = tableTest doubleQuotedString
                         [ (escapedString "", Just "")
                         , (escapedString "Hello, world!", Just "Hello, world!")
                         , (escapedString [backslashCr, quoteCh], Just [backslashCr, quoteCh])
                         , (escapedString [backslashCr, 'n'], Just [backslashCr, 'n'])
                         ]

intLitExpr = ExpressionLit . LiteralInt
flLitExpr = ExpressionLit . LiteralFloat

testExpression = tableTest expression
                 [ ("1", Just (intLitExpr 1))
                 , ("(123)", Just (ExpressionParen (intLitExpr 123)))
                 , ("var1", Just (ExpressionVar "var1"))
                 , ("barFn()", Just (ExpressionFnCall "barFn" []))
                 , ("1 + foo  ( 3 * 4 , bar(\"zig\") ) / 5",
                    Just (ExpressionBinary Plus (intLitExpr 1)
                         (ExpressionBinary Divide
                           (ExpressionFnCall "foo"
                            [ExpressionBinary Times (intLitExpr 3) (intLitExpr 4),
                             ExpressionFnCall "bar"
                             [ExpressionLit (LiteralString "zig")]])
                           (intLitExpr 5))))
                 , ("~-123", Just (ExpressionUnary Flip (ExpressionUnary Negate (intLitExpr 123))))
                 , ("~(1 + 2)", Just (ExpressionUnary Flip
                                      (ExpressionParen (ExpressionBinary Plus
                                                        (intLitExpr 1) (intLitExpr 2)))))
                 , ("-1 + 3", Just (ExpressionBinary Plus
                                    (ExpressionUnary Negate (intLitExpr 1))
                                    (intLitExpr 3)))
                 , ("3 + -1", Just (ExpressionBinary Plus
                                    (intLitExpr 3)
                                    (ExpressionUnary Negate (intLitExpr 1))))
                 , ("3.14 ^ 2.0", Just (ExpressionBinary Power (flLitExpr 3.14) (flLitExpr 2.0)))
                 ]

testFnCall = tableTest (functionCallExpression "foo")
             [ ("(    )", Just (ExpressionFnCall "foo" []))
             , ("()", Just (ExpressionFnCall "foo" []))
             , ("(123)", Just (ExpressionFnCall "foo" [intLitExpr 123]))
             , ("(   123  )", Just (ExpressionFnCall "foo" [intLitExpr 123]))
             , ("  (123)", Just (ExpressionFnCall "foo" [intLitExpr 123]))
             ]

testFnArgs = tableTest fnCallArgs
            [ ("()", Just [])
            , ("( )", Just [])
            , ("(  )", Just [])
            , ("(123)", Just [intLitExpr 123])
            , ("( 123)", Just [intLitExpr 123])
            , ("(123 )", Just [intLitExpr 123])
            , ("(  123  )", Just [intLitExpr 123])
            , ("(123,456)", Just [intLitExpr 123, intLitExpr 456])
            , ("(123,  456)", Just [intLitExpr 123, intLitExpr 456])
            , ("(123  ,456)", Just [intLitExpr 123, intLitExpr 456])
            , ("(,)", Nothing)
            , ("(,123)", Nothing)
            , ("(123,)", Nothing)
            , ("(123, ,456)", Nothing)
            , ("(123 456)", Nothing)
            ]

testBinaryExpr = tableTest binaryExpression
                 [ ("1+2", Just (ExpressionBinary Plus (intLitExpr 1) (intLitExpr 2)))
                 , ("1  -  2", Just (ExpressionBinary Minus (intLitExpr 1) (intLitExpr 2)))
                 , ("1 + (2 * 3)", Just (ExpressionBinary Plus (intLitExpr 1)
                                         (ExpressionParen (ExpressionBinary Times
                                                           (intLitExpr 2) (intLitExpr 3)))))
                 , ("2 + 3 * 4", Just (ExpressionBinary Plus (intLitExpr 2)
                                       (ExpressionBinary Times (intLitExpr 3) (intLitExpr 4))))
                 , ("2 * 3 + 4", Just (ExpressionBinary Plus
                                       (ExpressionBinary Times (intLitExpr 2) (intLitExpr 3))
                                       (intLitExpr 4)))
                 , ("True && False", Just (ExpressionBinary And
                                            (ExpressionLit (LiteralStruct "True"))
                                            (ExpressionLit (LiteralStruct "False"))))
                 ]

testStatements = tableTest statement
                 [ ("let abc = 123", Just (StatementLet "abc" (intLitExpr 123)))
                 , ("println(\"test\")", Just (StatementExpr (ExpressionFnCall "println"
                                                              [ExpressionLit (LiteralString "test")])))
                 , ("last(foo)", Just (StatementExpr (ExpressionFnCall "last"
                                                      [ExpressionVar "foo"])))
                 , ("return 5", Just (StatementReturn (intLitExpr 5)))
                 , ("if True { return 1 }", Just (StatementIf
                                                  { condition=(ExpressionLit (LiteralStruct "True"))
                                                  , body=[StatementReturn (intLitExpr 1)]
                                                  , elseIfBlocks=[]
                                                  , elseBlock=Nothing }))
                 , ("while 10 { let i = 20 }", Just $ StatementWhile
                                                         (intLitExpr 10)
                                                         [StatementLet "i" (intLitExpr 20)])
                 ]

testBlock = tableTest block
            [ ("{}", Just [])
            , ("{1}", Just [StatementExpr (intLitExpr 1)])
            , ("{  1  }", Just [StatementExpr (intLitExpr 1)])
            , ("{\n1\n}", Just [StatementExpr (intLitExpr 1)])
            , ("{\n  1\n  return 2;  }", Just [ StatementExpr (intLitExpr 1)
                                              , StatementReturn (intLitExpr 2)])
            , ("{1;return 2;}", Just [ StatementExpr (intLitExpr 1)
                                     , StatementReturn (intLitExpr 2)])
            ]

testTypeDef = tableTest typeDef
              [ ("()", Just NilType)
              , ("(  )", Nothing)
              , ("Bool", Just $ NamedType "Bool" [])
              , ("Bool[]", Just $ NamedType "Bool" [])
              , ("List[a]", Just $ NamedType "List" [TypeVar "a"])
              , ("List[  a  ]", Just $ NamedType "List" [TypeVar "a"])
              , ("Map[String, Set[Int]]", Just $ NamedType "Map" [ NamedType "String" []
                                                                 , NamedType "Set" [NamedType "Int" []]
                                                                 ])
              ]

testFunctionDef = tableTest functionDef
                  [ ("fn f() {}", Just $ FunctionDef "f" [] Nothing [])
                  , ("fn add(a, b) {}", Just $ FunctionDef "add"
                                                             [FnArg "a" Nothing, FnArg "b" Nothing]
                                                             Nothing
                                                             [])
                  , ("fn five() { return 1 + 4 }", Just $ FunctionDef "five" [] Nothing
                                                             [StatementReturn $ ExpressionBinary
                                                               Plus (intLitExpr 1) (intLitExpr 4)])
                  , ("fn head(l List[a]) a {}", Just $ FunctionDef "head"
                                                       [FnArg "l" (Just $ NamedType "List" [TypeVar "a"])]
                                                       (Just $ TypeVar "a")
                                                       [])
                  ]
