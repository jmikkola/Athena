module Main where

import Control.Monad (liftM)
import qualified Data.Map as Map
import Test.HUnit ( Assertion, Test( TestList ), (~:), (~?=) )
import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import Parse
import Eval

main = defaultMain $
       asGroup [ ("expressions", testExpressions)
               , ("scopes", testScopes)
               , ("while", testWhile)
               ]

intLiteral = ExpressionLit . LiteralInt
floatLiteral = ExpressionLit . LiteralFloat
strLiteral = ExpressionLit . LiteralString
varExpr = ExpressionVar

testExpressions =
  let makeTest (name, input, expectation) = name ~: assertEvals input expectation
  in TestList $ map makeTest [ ("an int", intLiteral 123, Just $ IntVal 123)
                             , ("a float", floatLiteral 123.4, Just $ FloatVal 123.4)
                             , ("a variable", varExpr "anInt", Just $ IntVal 123)
                             , ("null var", varExpr "foo", Nothing)
                             , ("str value", strLiteral "asdf", Just $ StringVal "asdf")
                             , ("struct value", ExpressionStruct "True" [], Just $ StructVal "True" [])
                             , ("unary -", ExpressionUnary Negate (intLiteral 123), Just $ IntVal (-123))
                             , ("unary ~", ExpressionUnary Flip (intLiteral 123), Just $ IntVal (-124))
                             , ("float ~", ExpressionUnary Flip (floatLiteral 123), Nothing)
                             , ( "float +"
                               , ExpressionBinary Plus (floatLiteral 5.0) (floatLiteral 1.5)
                               , Just $ FloatVal 6.5)
                             , ( "integer %"
                               , ExpressionBinary Mod (intLiteral 123) (intLiteral 10)
                               , Just $ IntVal 3)
                             , ( "mixed +"
                               , ExpressionBinary Plus (intLiteral 5) (floatLiteral 1.5)
                               , Nothing)
                             , ( "struct expr"
                               , ExpressionStruct "Pair" [ (ExpressionBinary Plus
                                                            (intLiteral 100) (intLiteral 42))
                                                         , (varExpr "anInt")]
                               , Just $ StructVal "Pair" [(IntVal 142), (IntVal 123)])
                             , ( "not True"
                               , ExpressionUnary Not (ExpressionStruct "True" [])
                               , Just $ StructVal "False" []
                               )
                             ]

intExpr n = ExpressionLit (LiteralInt n)
boolExpr b = ExpressionStruct (if b then "True" else "False") []

testScopes =
  let makeTest (name, blk, expectedScope) = name ~: assertScopeIs blk expectedScope
  in TestList $ map makeTest [ ("let works",
                                [StatementLet "foo" (intExpr 10)],
                                Just [("foo", IntVal 10)])
                             , ("assignment requires let",
                                [StatementAssign "foo" (intExpr 10)],
                                Nothing)
                             , ("assignment works after let",
                                [ StatementLet "foo" (intExpr 10)
                                , StatementAssign "foo" (intExpr 20)
                                , StatementAssign "foo" (intExpr 30)],
                                Just [("foo", IntVal 30)])
                             , ("single let of a given var",
                                [ StatementLet "foo" (intExpr 10)
                                , StatementLet "foo" (intExpr 20)],
                                Nothing)
                             , ("let in child scope is hidden",
                                [ StatementLet "foo" (intExpr 10)
                                , StatementIf (boolExpr True) (Block [ StatementLet "foo" (intExpr 20)
                                                                     ]) NoElse],
                                Just [("foo", IntVal 10)])
                             , ("new lets in child scope are hidden",
                                [ StatementLet "foo" (intExpr 10)
                                , StatementIf (boolExpr True) (Block [ StatementLet "bar" (intExpr 20)
                                                                     ]) NoElse],
                                Just [("foo", IntVal 10)])
                             , ("assignment in a child scope is visible",
                                [ StatementLet "foo" (intExpr 10)
                                , StatementIf (boolExpr True) (Block [ StatementAssign "foo" (intExpr 20)
                                                                     ]) NoElse],
                                Just [("foo", IntVal 20)])
                             , ("child let shadows parent var",
                                [ StatementLet "foo" (intExpr 10)
                                , StatementIf (boolExpr True) (Block [ StatementLet "foo" (intExpr 20)
                                                                     , StatementAssign "foo" (intExpr 30)
                                                                     ]) NoElse],
                                Just [("foo", IntVal 10)])
                             ]

var = ExpressionVar

testWhile =
  let makeTest (name, blk, expectedScope) = name ~: assertScopeIs blk expectedScope
  in TestList $ map makeTest
     [ ("fib sequence",
        [ StatementLet "i" (intExpr 10)
        , StatementLet "a" (intExpr 0)
        , StatementLet "b" (intExpr 1)
        , StatementWhile (ExpressionBinary Greater (var "i") (intExpr 0))
                         (Block [ StatementAssign "i" (ExpressionBinary Minus (var "i") (intExpr 1))
                                , StatementLet "temp" (ExpressionVar "a")
                                , StatementAssign "a" (ExpressionBinary Plus (var "a") (var "b"))
                                , StatementAssign "b" (var "temp")])],
        Just [("a", IntVal 55), ("b", IntVal 34), ("i", IntVal 0)])
     ]

exampleContext :: EvalContext
exampleContext = EvalContext $ Map.fromList [ ("anInt", IntVal 123)
                                            , ("aFloat", FloatVal 123.4)
                                            ]

assertEvals :: Expression -> Maybe Value  -> Test
assertEvals expr expected = (tryEvalExpr expr) ~?= expected

tryEvalExpr :: Expression -> Maybe Value
tryEvalExpr expr = case evalExpression exampleContext expr of
  Left _  -> Nothing
  Right e -> Just e

assertScopeIs :: [Statement] -> Maybe [(String, Value)] -> Test
assertScopeIs stmts resultScope = (tryEvalStmts stmts) ~?= (liftM (EvalContext . Map.fromList) $ resultScope)

tryEvalStmts :: [Statement] -> Maybe EvalContext
tryEvalStmts stmts = case evalStmts emptyContext stmts of
  Left  _        -> Nothing
  Right (ctx, _) -> Just ctx
