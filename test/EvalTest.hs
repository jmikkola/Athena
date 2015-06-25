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

testExpressions =
  let makeTest (name, input, expectation) = name ~: assertEvals input expectation
  in TestList $ map makeTest [ ("an int", IntValExpr 123, Just $ IntValExpr 123)
                             , ("a float", FloatValExpr 123.4, Just $ FloatValExpr 123.4)
                             , ("a variable", VarExpr "anInt", Just $ IntValExpr 123)
                             , ("null var", VarExpr "foo", Nothing)
                             , ("str value", StringValExpr "asdf", Just $ StringValExpr "asdf")
                             , ("struct value", StructValExpr "True" [], Just $ StructValExpr "True" [])
                             , ("unary -", UnaryExpr Negate (IntValExpr 123), Just $ IntValExpr (-123))
                             , ("unary ~", UnaryExpr Flip (IntValExpr 123), Just $ IntValExpr (-124))
                             , ("float ~", UnaryExpr Flip (FloatValExpr 123), Nothing)
                             , ( "float +"
                               , BinaryExpr Plus (FloatValExpr 5.0) (FloatValExpr 1.5)
                               , Just $ FloatValExpr 6.5)
                             , ( "integer %"
                               , BinaryExpr Mod (IntValExpr 123) (IntValExpr 10)
                               , Just $ IntValExpr 3)
                             , ( "mixed +"
                               , BinaryExpr Plus (IntValExpr 5) (FloatValExpr 1.5)
                               , Nothing)
                             , ( "struct expr"
                               , StructValExpr "Pair" [ (BinaryExpr Plus (IntValExpr 100) (IntValExpr 42))
                                                      , (VarExpr "anInt")]
                               , Just $ StructValExpr "Pair" [(IntValExpr 142), (IntValExpr 123)])
                             ]

testContext :: EvalContext
testContext = Map.fromList [("anInt", IntValExpr 123), ("aFloat", FloatValExpr 123.4)]

assertEvals :: Expr -> Maybe Expr  -> Test
assertEvals expr expected = (tryEvalExpr expr) ~?= expected

tryEvalExpr :: Expr -> Maybe Expr
tryEvalExpr expr = case evalExpr testContext expr of
  Left _  -> Nothing
  Right e -> Just e
