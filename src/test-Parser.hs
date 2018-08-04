module Main where


import Text.Parsec (eof, parse)
import Text.Parsec.String (Parser)

import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T
import Parser


import UnitTest
  ( Assertion
  , Test
  , assertRight
  , assertLeft
  , runTests
  , test )

main = do
  putStrLn "Testing Parser"
  runTests tests

boolT :: T.TypeDecl
boolT = T.TypeName "Bool"
intT :: T.TypeDecl
intT = T.TypeName "Int"
stringT :: T.TypeDecl
stringT = T.TypeName "String"
nilT :: T.TypeDecl
nilT = T.TypeName "()"

type Val = E.Value ()
type Expr = E.Expression ()
type Stmt = S.Statement ()

floatVal :: Float -> Val
floatVal f = E.FloatVal () f

intVal :: Int -> Val
intVal i = E.IntVal () i

boolVal :: Bool -> Val
boolVal b = E.BoolVal () b

strVal :: String -> Val
strVal s = E.StrVal () s

eVal :: Val -> Expr
eVal value = E.Val () value

eVar :: String -> Expr
eVar name = E.Var () name

eCall :: Expr -> [Expr] -> Expr
eCall fn args = E.Call () fn args

eBinary :: E.BinOp -> Expr -> Expr -> Expr
eBinary op l r = E.Binary () op l r

sLet :: String -> T.Type -> Expr -> Stmt
sLet name t e = S.Let () name t e

sBlock :: [Stmt] -> Stmt
sBlock stmts = S.Block () stmts

sReturn :: Maybe Expr -> Stmt
sReturn me = S.Return () me

sAssign :: [String] -> Expr -> Stmt
sAssign path e = S.Assign () path e

tests :: [Test]
tests =
  -- expressions
  [ expectParses numberParser "123.345" (floatVal 123.345)
  , expectParses valueParser "123.345" (floatVal 123.345)
  , expectParses expressionParser "123.345" (eVal (floatVal 123.345))
  , expectParses expressionParser "a123" (eVar "a123")
  , expectParses expressionParser "f(a)" (eCall (eVar "f") [eVar "a"])
  , expectParses expressionParser "f(a, b , c )"
    (eCall (eVar "f") [eVar "a", eVar "b", eVar "c"])
  , expectParses expressionParser "Bool(a)" (E.Cast () "Bool" (eVar "a"))
  , expectParses expressionParser "(2 + 3)"
    (E.Paren () (eBinary E.Plus (eVal (intVal 2)) (eVal (intVal 3))))
  , expectParses expressionParser "Point{\nx: 123,\ny: 45, \n}"
    (eVal $ E.StructVal () "Point"
     [("x", eVal $ intVal 123), ("y", eVal $ intVal 45)])
  , expectParses expressionParser "int ** 3"
    (eBinary E.Power (eVar "int") (eVal (intVal 3)))
  , expectParses expressionParser "\"a quoted \\\"string\\\" \""
    (eVal (strVal "a quoted \"string\" "))
  , expectParses expressionParser "!False"
    (E.Unary () E.BoolNot (eVal (boolVal False)))
  , expectParses expressionParser "foo.bar"
    (E.Access () (eVar "foo") "bar")
  , expectParses expressionParser "foo.bar.baz"
    (E.Access () (E.Access () (eVar "foo") "bar") "baz")
  , expectParses expressionParser "1 + 2 * 3 + 4"
    (eBinary E.Plus
      (eVal $ intVal 1)
      (eBinary E.Plus
        (eBinary E.Times (eVal $ intVal 2) (eVal $ intVal 3))
        (eVal $ intVal 4)))
  , expectParses expressionParser "1 == 1 && 2 < 3"
    (eBinary E.BoolAnd
     (eBinary E.Eq (eVal (intVal 1)) (eVal (intVal 1)))
     (eBinary E.Less (eVal (intVal 2)) (eVal (intVal 3))))
  , expectParses typeParser "Int" "Int"
  , expectParses typeDefParser "struct {\n  a  Int\nb String\n}"
    (T.Struct [("a", intT), ("b", stringT)])
  , testEnumType
  , testEnumType2

    -- statements
  , expectParses statementParser "return \"foo\""
    (sReturn $ Just $ eVal $ strVal "foo")
  , expectParses statementSep "\n" ()
  , expectParses statementSep "  \n  " ()
  , expectParses statementSep "  \n\n  \n  " ()
  , expectParses statementParser "{\n}" (sBlock [])
  , expectParses statementParser "{\nreturn 1\n}"
    (sBlock [(sReturn $ Just $ eVal $ intVal 1)])
  , expectParses statementParser "{\n  return 1\n}"
    (sBlock [(sReturn $ Just $ eVal $ intVal 1)])
  , expectParses statementParser "{\nreturn 1  \n}"
    (sBlock [(sReturn $ Just $ eVal $ intVal 1)])
  , expectParses statementParser "{  \n  return 1  \n  \n }"
    (sBlock [(sReturn $ Just $ eVal $ intVal 1)])
  , expectParses statementParser "{\n{\n}\n{\n}\n}"
    (sBlock [sBlock [], sBlock []])
  , expectParses statementParser "let a123 Bool = True"
    (sLet "a123" "Bool" (eVal $ boolVal True))
  , expectParses statementParser "a.b.c = True"
    (sAssign ["a", "b", "c"] (eVal $ boolVal True))
  , expectParses statementParser "print(c)"
    (S.Expr () $ eCall (eVar "print") [(eVar "c")])
  , expectParses letStatement "let int Int = 5 + (2 * 10) / 3 % 4"
    (sLet "int" "Int" (eBinary E.Plus
                        (eVal (intVal 5))
                        (eBinary E.Divide (E.Paren ()
                                             (eBinary E.Times
                                              (eVal (intVal 2))
                                              (eVal (intVal 10))))
                         (eBinary E.Mod
                          (eVal (intVal 3))
                           (eVal (intVal 4))))))
  , expectParses assignStatement "int = 3"
    (sAssign ["int"] $ eVal $ intVal 3)
  , expectParses assignStatement "int = int ** 3"
    (sAssign ["int"] $ eBinary E.Power (eVar "int") (eVal $ intVal 3))
  -- blocks and larger
  , testParsingBlock
  , testParsingIf
--  , testParsingMatch
  , testParsingFunc
  , testParsingFunc2
  , testParsingTypeDecl
  ]

testEnumType :: Test
testEnumType =
  let text = "enum {\n Cons {\n item Int \n next List \n } \n End \n }"
      expected = T.Enum [ ("Cons", [("item", intT), ("next", T.TypeName "List")])
                        , ("End", []) ]
  in expectParses typeDefParser text expected

testEnumType2 :: Test
testEnumType2 =
  let text = "enum {\n TInt\n TFloat \n }"
      expected = T.Enum [ ("TInt", [])
                        , ("TFloat", []) ]
  in expectParses typeDefParser text expected

testParsingBlock :: Test
testParsingBlock =
  let text = "{\n  let a1 Bool = True \n  return a1  \n }"
      expected = sBlock [ sLet "a1" "Bool" (eVal $ boolVal True)
                         , sReturn (Just $ eVar "a1")
                         ]
  in expectParses statementParser text expected

testParsingIf :: Test
testParsingIf =
  let text = "if a == 1 {\nreturn a\n}"
      test = eBinary E.Eq (eVar "a") (eVal $ intVal 1)
      body = [sReturn $ Just $ eVar "a"]
      expected = S.If () test body Nothing
  in expectParses ifStatement text expected

{-
testParsingMatch :: Test
testParsingMatch =
  let text = "match x {\n  _ {\nreturn 1\n}\n  Link(_, next) {\n return 2\n}\n}"
      ret n = sBlock [sReturn $ Just $ eVal $ intVal n]
      case1 = S.MatchCase S.MatchAnything (ret 1)
      case2 = S.MatchCase (S.MatchStructure "Link" [S.MatchAnything, S.MatchVariable "next"]) (ret 2)
      expected = S.Match (eVar "x") [case1, case2]
  in expectParses statementParser text expected
-}

testParsingFunc :: Test
testParsingFunc =
  let text = "fn main() {\n}"
      expected = D.Function () "main" (T.Function [] nilT) [] (sBlock [])
  in expectParses declarationParser text expected

testParsingFunc2 :: Test
testParsingFunc2 =
  let text = "fn main(a Int, b Bool) Bool {\n//a comment\n}"
      fnType = T.Function [intT, boolT] boolT
      expected = D.Function () "main" fnType ["a", "b"] (sBlock [])
  in expectParses declarationParser text expected

testParsingTypeDecl :: Test
testParsingTypeDecl =
  let text = "type Foo struct {\n  asdf Int\n  xyz Foo\n}"
      declaredType = T.Struct [("asdf", intT), ("xyz", T.TypeName "Foo")]
      expected = D.TypeDef () "Foo" declaredType
  in expectParses declarationParser text expected

---- Utilities ----

expectParses :: (Eq a, Show a) => Parser a -> String -> a -> Test
expectParses parser text expected =
  case parse (parser <* eof) "<test>" text of
   (Left err) -> do
     putStrLn $  "failed parsing ''" ++ text ++ "'', error parsing (" ++ show err ++ ")"
     return False
   (Right result) ->
     if result == expected
     then do
       return True
     else do
       putStrLn $ "failed parsing ''" ++ text ++ "'', expected " ++ show expected ++ ", got " ++ show result
       return False
