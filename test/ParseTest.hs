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
                             , ("if statement", testIfStatement)
                             , ("block", testBlock)
                             , ("type def", testTypeDef)
                             , ("function", testFunctionDef)
                             , ("file", testFile)
                             , ("lineComment", testLineComment)
                             , ("blockComment", testBlockComment)
                             , ("Comment in whitespace", testCommentInWhitespace)
                             , ("test match patterns", testMatchPattern)
                             , ("test match case", testMatchCase)
                             , ("Test match statement", testMatchStatement)
                             ]

maybeEqEither :: (Eq a) => Maybe a -> Either l a -> Bool
maybeEqEither (Just a) (Right b) = a == b
maybeEqEither Nothing  (Left _)  = True
maybeEqEither _        _         = False

assertParses :: (Eq a, Show a) => Parser a -> String -> Maybe a -> Assertion
assertParses parser input expectation = assertBool message matched
  where result    = parse (parser <* anyWhitespace <* eof) "test" input
        matched   = maybeEqEither expectation result
        resultStr = case expectation of
          Nothing -> "not parse"
          Just r  -> "parse to " ++ show r
        message   = "expected " ++ show input ++ " to " ++ resultStr ++ ", got " ++ show result

tableTest parseFn cases = TestList $ map makeTest cases
  where makeTest (input, expectation) = input ~: assertParses parseFn input expectation

undisplay :: (Display a) => a -> (String, Maybe a)
undisplay a = (display a, Just a)

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

trueExpr = ExpressionStruct "True" []
falseExpr = ExpressionStruct "False" []

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
               , (undisplay $ LiteralChar 'c')
               , (undisplay $ LiteralChar '\\')
               , (undisplay $ LiteralChar '\n')
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

exampleIfExpr = ExpressionIf (intLitExpr 1) (intLitExpr 2) (intLitExpr 3)

exampleMathExpr = ExpressionBinary Plus (intLitExpr 1)
                  (ExpressionBinary Divide (intLitExpr 3) (intLitExpr 5))

testExpression = tableTest expression
                 [ ("1", Just (intLitExpr 1))
                 , ("(123)", Just (ExpressionParen (intLitExpr 123)))
                 , ("var1", Just (ExpressionVar "var1"))
                 , ("barFn()", Just (ExpressionFnCall "barFn" []))
                 , ("1 + 3", Just (ExpressionBinary Plus (intLitExpr 1) (intLitExpr 3)))
                 , ("1 + 3 / 5", Just exampleMathExpr)
                 , ("1 /* comment */ + 3 / 5 /* another comment! */", Just exampleMathExpr)
                 , (undisplay $ exampleMathExpr)
                 , ("1+3/5", Just exampleMathExpr)
                 , ("bar(\"zig\")",
                    Just (ExpressionFnCall "bar" [ExpressionLit (LiteralString "zig")]))
                 , ("foo( 3 * 4 )",
                    Just (ExpressionFnCall "foo" [ExpressionBinary Times (intLitExpr 3) (intLitExpr 4)]))
                 , ("1 + foo( 3 * 4 /* comment */ , bar(\"zig\") ) / 5",
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
                 , ("i", Just (ExpressionVar "i"))
                 , ("i_am_a_variable", Just (ExpressionVar "i_am_a_variable"))
                 , ("am_I_a_var?", Just (ExpressionVar "am_I_a_var?"))
                 , ("(i)", Just (ExpressionParen (ExpressionVar "i")))
                 , ("1 + bar", Just (ExpressionBinary Plus (intLitExpr 1) (ExpressionVar "bar")))
                 , ("i < 5", Just (ExpressionBinary Less
                                    (ExpressionVar "i")
                                    (intLitExpr 5)))
                 , ("Pair( 1 + 5, True || False )",
                    Just (ExpressionStruct "Pair"
                          [ ExpressionBinary Plus (intLitExpr 1) (intLitExpr 5)
                          , ExpressionBinary Or trueExpr falseExpr]))
                 , (undisplay $ ExpressionStruct "Pair"
                    [ ExpressionBinary Plus (intLitExpr 1) (intLitExpr 5)
                    , ExpressionBinary Or trueExpr falseExpr])
                 , ("False", Just (ExpressionStruct "False" []))
                 , (undisplay $ ExpressionStruct "True" [])
                 , (undisplay $ ExpressionStruct "Pbair" [(intLitExpr 23), trueExpr])
                 , (undisplay $ ExpressionUnary Not (ExpressionStruct "True" []))
                 , (undisplay $ exampleIfExpr)
                 , (undisplay $ ExpressionIf (intLitExpr 11) (intLitExpr 12) exampleIfExpr)
                 , (undisplay $ ExpressionIf (intLitExpr 11)  exampleIfExpr  (intLitExpr 13))
                 ]

testFnCall = tableTest (functionCallExpression "foo")
             [ ("(    )", Just (ExpressionFnCall "foo" []))
             , ("()", Just (ExpressionFnCall "foo" []))
             , ("(123)", Just (ExpressionFnCall "foo" [intLitExpr 123]))
             , ("(   123  )", Just (ExpressionFnCall "foo" [intLitExpr 123]))
             , ("( /* comment */  123  )", Just (ExpressionFnCall "foo" [intLitExpr 123]))
             , ("  (123)", Nothing)
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
            , ("(123, /* comment */  456)", Just [intLitExpr 123, intLitExpr 456])
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
                                            (ExpressionStruct "True" [])
                                            (ExpressionStruct "False" [])))
                 ]

testStatements = tableTest statement
                 [ ("let abc = 123", Just (StatementLet "abc" (intLitExpr 123)))
                 , ("println(\"test\")", Just (StatementExpr (ExpressionFnCall "println"
                                                              [ExpressionLit (LiteralString "test")])))
                 , ("last(foo)", Just (StatementExpr (ExpressionFnCall "last"
                                                      [ExpressionVar "foo"])))
                 , ("return 5", Just (StatementReturn (intLitExpr 5)))
                 , ("if True { return 1 }", Just (StatementIf (ExpressionStruct "True" [])
                                                   (Block [StatementReturn (intLitExpr 1)])
                                                   NoElse))
                 , ("while i < 10 { i = i + 1 }", Just $ StatementWhile
                                                         (ExpressionBinary Less
                                                          (ExpressionVar "i") (intLitExpr 10))
                                                          (Block [StatementAssign "i"
                                                            (ExpressionBinary Plus
                                                             (ExpressionVar "i") (intLitExpr 1))]))
                 , (undisplay (StatementWhile exampleBoolExpr
                                              (Block [StatementAssign "i"
                                                      (ExpressionBinary Plus
                                                       (ExpressionVar "i") (intLitExpr 1))])))
                 , (undisplay (StatementFor "x" exampleBoolExpr exampleBlock))
                 , (undisplay $ StatementFn $ exampleFnDef)
                 , (undisplay exampleMatchStatement)
                 ]

exampleBoolExpr = (ExpressionBinary Less (ExpressionVar "i") (intLitExpr 10))
exampleBlock = (Block [StatementReturn (intLitExpr 123)])

testIfStatement = tableTest ifStatement
                  [ (undisplay $ StatementIf exampleBoolExpr exampleBlock NoElse)
                  , (undisplay $ StatementIf exampleBoolExpr exampleBlock
                                 (Else exampleBlock))
                  , (undisplay $ StatementIf exampleBoolExpr exampleBlock
                                 (ElseIf exampleBoolExpr exampleBlock NoElse))
                  , (undisplay $ StatementIf exampleBoolExpr exampleBlock
                                 (ElseIf exampleBoolExpr exampleBlock
                                   (ElseIf exampleBoolExpr exampleBlock
                                     (Else exampleBlock))))
                  , ("if 1 { 5 } else if 2 { 3 } else { 999 }",
                     Just $ StatementIf (intLitExpr 1)
                         (Block [StatementExpr $ intLitExpr 5])
                         (ElseIf (intLitExpr 2)
                                 (Block [StatementExpr $ intLitExpr 3])
                                 (Else $ Block [StatementExpr $ intLitExpr 999])))
                  ]

testBlock = tableTest block
            [ ("{}", Just $ Block [])
            , ("{1}", Just $ Block [StatementExpr (intLitExpr 1)])
            , ("{  1  }", Just $ Block [StatementExpr (intLitExpr 1)])
            , ("{\n1\n}", Just $ Block [StatementExpr (intLitExpr 1)])
            , ("{\n  1\n  return 2;  }", Just $ Block [ StatementExpr (intLitExpr 1)
                                                      , StatementReturn (intLitExpr 2)])
            , ("{1;return 2;}", Just $ Block [ StatementExpr (intLitExpr 1)
                                             , StatementReturn (intLitExpr 2)])
            , (undisplay (Block [ StatementExpr (intLitExpr 1)
                                , StatementReturn (intLitExpr 2)]))
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
              , (undisplay (NamedType "Map" [ NamedType "String" []
                                            , NamedType "Set" [NamedType "Int" []]
                                            ]))
              ]

exampleFnDef = FunctionDef "cons"
                      [ FnArg "item" (Just $ TypeVar "a")
                      , FnArg "rest" (Just $ NamedType "List" [TypeVar "a"])
                      ]
                      (Just $ NamedType "List" [TypeVar "a"])
                      (Block [StatementReturn $ ExpressionBinary Plus
                              (ExpressionVar "item") (ExpressionVar "rest")])

exampleShortFn = ShortFn "head" [FnArg "lst" Nothing] Nothing
                   (ExpressionFnCall "lst" [ExpressionVar "consFirst"])

testFunctionDef = tableTest functionDef
                  [ ("fn f() {}", Just $ FunctionDef "f" [] Nothing (Block []))
                  , ("fn add(a, b) {}", Just $ FunctionDef "add"
                                                             [FnArg "a" Nothing, FnArg "b" Nothing]
                                                             Nothing
                                                             (Block []))
                  , ("fn five() {\nreturn 1 + 4\n}", Just $ FunctionDef "five" [] Nothing
                                                          (Block  [StatementReturn $ ExpressionBinary
                                                                   Plus (intLitExpr 1) (intLitExpr 4)]))
                  , ("fn head(l List[a]) a {}", Just $ FunctionDef "head"
                                                       [FnArg "l" (Just $ NamedType "List" [TypeVar "a"])]
                                                       (Just $ TypeVar "a")
                                                       (Block  []))
                  , (undisplay $ exampleFnDef)
                  , (undisplay $ FunctionDef "cons"
                      [FnArg "item" Nothing, FnArg "rest" Nothing] Nothing (Block []))
                  , (undisplay exampleShortFn)
                  ]

_arg s = FnArg s Nothing
_var s = ExpressionVar s

_addFn = StatementFn (ShortFn "add" [_arg "a", _arg "b"] Nothing
                      (ExpressionBinary Plus (_var "a") (_var "b")))

_barFn = StatementFn (FunctionDef "bar" [_arg "n"] Nothing
                      (Block [ StatementLet "m" (intLitExpr 3)
                             , StatementReturn (ExpressionBinary Times (_var "m") (_var "n"))]))

testFile = tableTest parseFile
           [ ("", Just $ [])
           , ("fn add(a, b) = a + b", Just $ [_addFn])
           , ("\n\nfn add(a, b) = a + b\n\n", Just $ [_addFn])
           , ("\n   \n\n fn   add(a, b) = a + b\n/* comment */fn bar(n) {\n\nlet m = 3\nreturn m * n\n}",
              Just $ [_addFn, _barFn])
           ]

testLineComment = tableTest parseLineComment
                  [ ("//\n", Just "//")
                  , ("// foo! ", Just "// foo! ")
                  , ("//////", Just "//////")
                  , ("//**/", Just "//**/")
                  , ("// not a block comment: /*", Just "// not a block comment: /*")
                  , ("// trailing newlines\n\n ", Just "// trailing newlines")
                  ]

testBlockComment = tableTest parseBlockComment
                   [ ("/**/", Just "/**/")
                   , ("/*  */ \n", Just "/*  */")
                   , ("/*\n*\n*/", Just "/*\n*\n*/")
                   , ("/***/", Just "/***/")
                   , ("/****/", Just "/****/")
                   ]

makeWhitespaceTest parseFn str = str ~: assertParses parseFn str (Just str)

testCommentInWhitespace = TestList $ map (makeWhitespaceTest any1Whitespace)
                           [ " "
                           , "/* foo */"
                           , "\n/* // foo\nbar*/  // still whitespace"
                           ]

testMatchPattern = tableTest parseMatchPattern
                   [ (undisplay $ UnderscorePattern)
                   , (undisplay $ VarPattern "foo")
                   , (undisplay $ StructPattern "True" [])
                   , ("Cons( _ )", Just $ StructPattern "Cons" [UnderscorePattern])
                   , (undisplay $ StructPattern "Cons" [ VarPattern "head"
                                                       , StructPattern "Nil" []])
                   , ("Cons(_, Cons(second, rest))",
                      Just $ StructPattern "Cons" [ UnderscorePattern
                                                  , StructPattern "Cons" [ VarPattern "second"
                                                                         , VarPattern "rest" ]])
                   , ("Cons()", Nothing)
                   , ("Cons (_)", Nothing)
                   , (undisplay $ LiteralPattern (LiteralInt 10))
                   ]

examplePattern = StructPattern "Cons" [UnderscorePattern, VarPattern "rest"]

testMatchCase = tableTest parseMatchCase
                [ ("True {}", Just $ MatchCase (StructPattern "True" []) (Block []))
                , (undisplay $ MatchCase examplePattern exampleBlock)
                ]

exampleMatchStatement = StatementMatch (ExpressionFnCall "foo" [])
                        [ MatchCase examplePattern exampleBlock
                        , MatchCase UnderscorePattern (Block [])]

testMatchStatement = tableTest parseMatchStatement
                     [ ("match x {\n  True {}\n  _ {}\n}",
                        Just $ StatementMatch (_var "x")
                               [ MatchCase (StructPattern "True" []) (Block [])
                               , MatchCase UnderscorePattern (Block [])])
                     , (undisplay exampleMatchStatement)
                     , ("match x {\n True {} _ {} }", Nothing)
                     ]
