module ParserTest where

import Control.Applicative ( (<*) )

import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser)

import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T
import Parser

testMain :: IO ()
testMain = do
  passing <- sequence tests
  let passCount = length [p | p <- passing, p]
  let totalCount = length passing
  putStrLn $ show passCount ++ "/" ++ show totalCount ++ " tests passed"

tests :: [IO Bool]
tests =
  -- expressions
  [ expectParses numberParser "123.345" (E.EFloat 123.345)
  , expectParses valueParser "123.345" (E.EFloat 123.345)
  , expectParses expressionParser "123.345" (E.EValue (E.EFloat 123.345))
  , expectParses expressionParser "a123" (E.EVariable "a123")
  , expectParses expressionParser "f(a)" (E.ECall (E.EVariable "f") [E.EVariable "a"])
  , expectParses expressionParser "f(a, b , c )"
    (E.ECall (E.EVariable "f") [E.EVariable "a", E.EVariable "b", E.EVariable "c"])
  , expectParses expressionParser "Bool(a)" (E.ECast T.Bool (E.EVariable "a"))
  , expectParses expressionParser "(2 + 3)"
    (E.EParen (E.EBinary E.Plus (E.EValue (E.EInt 2)) (E.EValue (E.EInt 3))))
  , expectParses expressionParser "\"a quoted \\\"string\\\" \""
    (E.EValue (E.EString "a quoted \"string\" "))
  , expectParses expressionParser "!False"
    (E.EUnary E.BoolNot (E.EValue (E.EBool False)))
  , expectParses expressionParser "1 + 2 * 3 + 4"
    (E.EBinary E.Plus
      (E.EValue $ E.EInt 1)
      (E.EBinary E.Plus
        (E.EBinary E.Times (E.EValue $ E.EInt 2) (E.EValue $ E.EInt 3))
        (E.EValue $ E.EInt 4)))
  , expectParses expressionParser "1 == 1 && 2 < 3"
    (E.EBinary E.BoolAnd
     (E.EBinary E.Eq (E.EValue (E.EInt 1)) (E.EValue (E.EInt 1)))
     (E.EBinary E.Less (E.EValue (E.EInt 2)) (E.EValue (E.EInt 3))))
  -- statements
  , expectParses statementParser "return \"foo\""
    (S.Return $ Just $ E.EValue $ E.EString "foo")
  , expectParses statementSep "\n" ()
  , expectParses statementSep "  \n  " ()
  , expectParses statementSep "  \n\n  \n  " ()
  , expectParses statementParser "{\n}" (S.Block [])
  , expectParses statementParser "{\nreturn 1\n}"
    (S.Block [(S.Return $ Just $ E.EValue $ E.EInt 1)])
  , expectParses statementParser "{\n  return 1\n}"
    (S.Block [(S.Return $ Just $ E.EValue $ E.EInt 1)])
  , expectParses statementParser "{\nreturn 1  \n}"
    (S.Block [(S.Return $ Just $ E.EValue $ E.EInt 1)])
  , expectParses statementParser "{  \n  return 1  \n  \n }"
    (S.Block [(S.Return $ Just $ E.EValue $ E.EInt 1)])
  , expectParses statementParser "{\n{\n}\n{\n}\n}"
    (S.Block [S.Block [], S.Block []])
  , expectParses statementParser "let a123 Bool = True"
    (S.Let "a123" T.Bool (E.EValue $ E.EBool True))
  , expectParses statementParser "print(c)"
    (S.Expr $ E.ECall (E.EVariable "print") [(E.EVariable "c")])
  , expectParses letStatement "let int Int = 5 + (2 * 10) / 3 % 4"
    (S.Let "int" T.Int (E.EBinary E.Plus
                        (E.EValue (E.EInt 5))
                        (E.EBinary E.Divide (E.EParen
                                             (E.EBinary E.Times
                                              (E.EValue (E.EInt 2))
                                              (E.EValue (E.EInt 10))))
                         (E.EBinary E.Mod
                          (E.EValue (E.EInt 3))
                           (E.EValue (E.EInt 4))))))
  -- blocks and larger
  , testParsingBlock
  , testParsingIf
  , testParsingFunc
  , testParsingFunc2
  ]

testParsingBlock :: IO Bool
testParsingBlock =
  let text = "{\n  let a1 Bool = True \n  return a1  \n }"
      expected = S.Block [ S.Let "a1" T.Bool (E.EValue $ E.EBool True)
                         , S.Return (Just $ E.EVariable "a1")
                         ]
  in expectParses statementParser text expected

testParsingIf :: IO Bool
testParsingIf =
  let text = "if a == 1 {\nreturn a\n}"
      test = E.EBinary E.Eq (E.EVariable "a") (E.EValue $ E.EInt 1)
      body = [S.Return $ Just $ E.EVariable "a"]
      expected = S.If test body Nothing
  in expectParses ifStatement text expected

testParsingFunc :: IO Bool
testParsingFunc =
  let text = "fn main() {\n}"
      expected = D.Function "main" (T.Function [] T.Nil) [] (S.Block [])
  in expectParses declarationParser text expected

testParsingFunc2 :: IO Bool
testParsingFunc2 =
  let text = "fn main(a Int, b Bool) Bool {\n//a comment\n}"
      fnType = T.Function [T.Int, T.Bool] T.Bool
      expected = D.Function "main" fnType ["a", "b"] (S.Block [])
  in expectParses declarationParser text expected

---- Utilities ----

expectParses :: (Eq a, Show a) => Parser a -> String -> a -> IO Bool
expectParses parser text expected =
  case parse (parser <* eof) "<test>" text of
   (Left err) -> do
     putStrLn $  "failed parsing ''" ++ text ++ "'', error parsing (" ++ show err ++ ")"
     return False
   (Right result) ->
     if result == expected
     then do
       putStrLn $ "pass"
       return True
     else do
       putStrLn $ "failed parsing ''" ++ text ++ "'', expected " ++ show expected ++ ", got " ++ show result
       return False
