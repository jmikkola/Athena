module ParserTest where

--import Control.Applicative ( (<*) )

import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser)

import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified Type as T
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
  [ expectParses numberParser "123.345" (E.FloatVal 123.345)
  , expectParses valueParser "123.345" (E.FloatVal 123.345)
  , expectParses expressionParser "123.345" (E.Val (E.FloatVal 123.345))
  , expectParses expressionParser "a123" (E.Var "a123")
  , expectParses expressionParser "f(a)" (E.Call (E.Var "f") [E.Var "a"])
  , expectParses expressionParser "f(a, b , c )"
    (E.Call (E.Var "f") [E.Var "a", E.Var "b", E.Var "c"])
  , expectParses expressionParser "Bool(a)" (E.Cast T.Bool (E.Var "a"))
  , expectParses expressionParser "(2 + 3)"
    (E.Paren (E.Binary E.Plus (E.Val (E.IntVal 2)) (E.Val (E.IntVal 3))))
  , expectParses expressionParser "Point{\nx: 123,\ny: 45, \n}"
    (E.Val $ E.StructVal "Point"
     [("x", E.Val $ E.IntVal 123), ("y", E.Val $ E.IntVal 45)])
  , expectParses expressionParser "int ** 3"
    (E.Binary E.Power (E.Var "int") (E.Val (E.IntVal 3)))
  , expectParses expressionParser "\"a quoted \\\"string\\\" \""
    (E.Val (E.StrVal "a quoted \"string\" "))
  , expectParses expressionParser "!False"
    (E.Unary E.BoolNot (E.Val (E.BoolVal False)))
  , expectParses expressionParser "foo.bar"
    (E.Access (E.Var "foo") "bar")
  , expectParses expressionParser "foo.bar.baz"
    (E.Access (E.Access (E.Var "foo") "bar") "baz")
  , expectParses expressionParser "1 + 2 * 3 + 4"
    (E.Binary E.Plus
      (E.Val $ E.IntVal 1)
      (E.Binary E.Plus
        (E.Binary E.Times (E.Val $ E.IntVal 2) (E.Val $ E.IntVal 3))
        (E.Val $ E.IntVal 4)))
  , expectParses expressionParser "1 == 1 && 2 < 3"
    (E.Binary E.BoolAnd
     (E.Binary E.Eq (E.Val (E.IntVal 1)) (E.Val (E.IntVal 1)))
     (E.Binary E.Less (E.Val (E.IntVal 2)) (E.Val (E.IntVal 3))))
  , expectParses typeParser "Int" T.Int
  , expectParses typeParser "struct {\n  a  Int\nb String\n}"
    (T.Struct [("a", T.Int), ("b", T.String)])
  , testEnumType
  , testEnumType2

    -- statements
  , expectParses statementParser "return \"foo\""
    (S.Return $ Just $ E.Val $ E.StrVal "foo")
  , expectParses statementSep "\n" ()
  , expectParses statementSep "  \n  " ()
  , expectParses statementSep "  \n\n  \n  " ()
  , expectParses statementParser "{\n}" (S.Block [])
  , expectParses statementParser "{\nreturn 1\n}"
    (S.Block [(S.Return $ Just $ E.Val $ E.IntVal 1)])
  , expectParses statementParser "{\n  return 1\n}"
    (S.Block [(S.Return $ Just $ E.Val $ E.IntVal 1)])
  , expectParses statementParser "{\nreturn 1  \n}"
    (S.Block [(S.Return $ Just $ E.Val $ E.IntVal 1)])
  , expectParses statementParser "{  \n  return 1  \n  \n }"
    (S.Block [(S.Return $ Just $ E.Val $ E.IntVal 1)])
  , expectParses statementParser "{\n{\n}\n{\n}\n}"
    (S.Block [S.Block [], S.Block []])
  , expectParses statementParser "let a123 Bool = True"
    (S.Let "a123" T.Bool (E.Val $ E.BoolVal True))
  , expectParses statementParser "a.b.c = True"
    (S.Assign ["a", "b", "c"] (E.Val $ E.BoolVal True))
  , expectParses statementParser "print(c)"
    (S.Expr $ E.Call (E.Var "print") [(E.Var "c")])
  , expectParses letStatement "let int Int = 5 + (2 * 10) / 3 % 4"
    (S.Let "int" T.Int (E.Binary E.Plus
                        (E.Val (E.IntVal 5))
                        (E.Binary E.Divide (E.Paren
                                             (E.Binary E.Times
                                              (E.Val (E.IntVal 2))
                                              (E.Val (E.IntVal 10))))
                         (E.Binary E.Mod
                          (E.Val (E.IntVal 3))
                           (E.Val (E.IntVal 4))))))
  , expectParses assignStatement "int = 3"
    (S.Assign ["int"] $ E.Val $ E.IntVal 3)
  , expectParses assignStatement "int = int ** 3"
    (S.Assign ["int"] $ E.Binary E.Power (E.Var "int") (E.Val $ E.IntVal 3))
  -- blocks and larger
  , testParsingBlock
  , testParsingIf
  , testParsingFunc
  , testParsingFunc2
  , testParsingTypeDecl
  ]

testEnumType :: IO Bool
testEnumType =
  let text = "enum {\n Cons {\n item Int \n next List \n } \n End \n }"
      expected = T.Enum [ ("Cons", [("item", T.Int), ("next", T.TypeName "List")])
                        , ("End", []) ]
  in expectParses typeParser text expected

testEnumType2 :: IO Bool
testEnumType2 =
  let text = "enum {\n TInt\n TFloat \n }"
      expected = T.Enum [ ("TInt", [])
                        , ("TFloat", []) ]
  in expectParses typeParser text expected

testParsingBlock :: IO Bool
testParsingBlock =
  let text = "{\n  let a1 Bool = True \n  return a1  \n }"
      expected = S.Block [ S.Let "a1" T.Bool (E.Val $ E.BoolVal True)
                         , S.Return (Just $ E.Var "a1")
                         ]
  in expectParses statementParser text expected

testParsingIf :: IO Bool
testParsingIf =
  let text = "if a == 1 {\nreturn a\n}"
      test = E.Binary E.Eq (E.Var "a") (E.Val $ E.IntVal 1)
      body = [S.Return $ Just $ E.Var "a"]
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

testParsingTypeDecl :: IO Bool
testParsingTypeDecl =
  let text = "type Foo struct {\n  asdf Int\n  xyz Foo\n}"
      declaredType = T.Struct [("asdf", T.Int), ("xyz", T.TypeName "Foo")]
      expected = D.TypeDef "Foo" declaredType
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
