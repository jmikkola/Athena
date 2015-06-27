module Main where

import qualified Data.Map as Map
import Test.HUnit ( Assertion, Test( TestList ), (~:), (~?=) )
import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import Parse
import Eval

main = defaultMain $
       asGroup [ ("expressions", testExpressions)
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

testContext :: EvalContext
testContext = Map.fromList [("anInt", IntVal 123)
                           , ("aFloat", FloatVal 123.4)
                           ]

assertEvals :: Expression -> Maybe Value  -> Test
assertEvals expr expected = (tryEvalExpr expr) ~?= expected

tryEvalExpr :: Expression -> Maybe Value
tryEvalExpr expr = case evalExpression testContext expr of
  Left _  -> Nothing
  Right e -> Just e
