module Read where

{-
This module is for turning raw SExpressions into function and other definitions.
Eventually, macro processing will be added before this step.
-}

import SExpression

type RName = String

data RFunction = RFunction
                 { name :: String
                 , lambda :: RLambda
                 }
               deriving (Eq, Show)

data RLambda = RLambda
               { args :: [String]
               , body :: RExpression
               }
             deriving (Eq, Show)

data RExpression = RFnCall
                   { calledFn :: RCalledFn
                   , expressions :: [RExpression]
                   }
                 | RValue { value :: RPlainValue }
                 deriving (Eq, Show)

data RCalledFn = RLambdaCall RLambda
               | RNamedCall RName
               deriving (Eq, Show)

data RPlainValue = RNil
                 | RTrue
                 | RChar Char
                 | RString String
                 | RKey String
                 | RSymbol RName
                 | RInt Int
                 | RFloat Float
                 deriving (Eq, Show)

sexprToValue :: SExpression -> Maybe RPlainValue
sexprToValue (SList []) = Just RNil
sexprToValue (SKey "nil") = Just RNil
sexprToValue (SKey "true") = Just RTrue
sexprToValue (SKey "t") = Just RTrue
sexprToValue (SKey key) = Just $ RKey key
sexprToValue (SSymbol sym) = Just $ RSymbol sym
sexprToValue (SCharValue c) = Just $ RChar c
sexprToValue (SStringValue s) = Just $ RString s
sexprToValue (SIntVal i) = Just $ RInt i
sexprToValue (SFloatVal f) = Just $ RFloat f
sexprToValue _ = Nothing
