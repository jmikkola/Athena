module SExpression where

data SExpression = SList [SExpression]
                 | SCharValue Char
                 | SStringValue String
                 | SKey String
                 | SSymbol String
                 | SIntVal Int
                 | SFloatVal Float
                 deriving (Eq, Show)
