module ParserTest where

import Control.Applicative ( (<*) )

import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser)

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
  [ expectParses numberParser "123.345" (E.EFloat 123.345)
  , expectParses valueParser "123.345" (E.EFloat 123.345)
  , expectParses expressionParser "123.345" (E.EValue (E.EFloat 123.345))
  , expectParses expressionParser "!False"
    (E.EUnary E.BoolNot (E.EValue (E.EBool False)))
  , expectParses expressionParser "1 + 2 * 3 + 4"
    (E.EBinary E.Plus
      (E.EValue $ E.EInt 1)
      (E.EBinary E.Plus
        (E.EBinary E.Times (E.EValue $ E.EInt 2) (E.EValue $ E.EInt 3))
        (E.EValue $ E.EInt 4)))
  , expectParses statementParser "return \"foo\""
    (S.Return $ Just $ E.EValue $ E.EString "foo")
  , expectParses statementSep "\n" ()
  , expectParses statementSep "  \n  " ()
  , expectParses statementSep "  \n\n  \n  " ()
  , expectParses blockStatements "}" []
  , expectParses blockStatement "{\n}" (S.Block [])
  , expectParses blockStatement "{\nreturn 1\n}"
    (S.Block [(S.Return $ Just $ E.EValue $ E.EInt 1)])
  , expectParses blockStatement "{\n  return 1\n}"
    (S.Block [(S.Return $ Just $ E.EValue $ E.EInt 1)])
  , expectParses blockStatement "{\nreturn 1  \n}"
    (S.Block [(S.Return $ Just $ E.EValue $ E.EInt 1)])
  , expectParses blockStatement "{  \n  return 1  \n  \n }"
    (S.Block [(S.Return $ Just $ E.EValue $ E.EInt 1)])
  , testParsingBlock
  ]

testParsingBlock :: IO Bool
testParsingBlock =
  let text = "{\n  let a1 Bool = True \n  return a1  \n }"
      expected = S.Block [ S.Let "a1" T.Bool (E.EValue $ E.EBool True)
                         , S.Return (Just $ E.EVariable "a1")
                         ]
  in expectParses statementParser text expected

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
