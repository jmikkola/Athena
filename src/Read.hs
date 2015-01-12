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

type RReader = Either String

readFunction :: SExpression -> RReader RFunction
readFunction (SList items) = case items of
  (SSymbol "defn":SSymbol fnName:SList args:body) -> do
    lbody <- readLambdaBody body
    largs <- readArgs args
    return $ RFunction fnName (RLambda largs lbody)
  _ -> Left ("Bad list for function def: " ++ (show items))
readFunction x             = Left ("Functions must be a list, got " ++ (show x))

readLambdaBody :: [SExpression] -> RReader RExpression
readLambdaBody []  = Left "Function has no body"
readLambdaBody [b] = do
  body <- expectList b
  readExpression (SList body)
readLambdaBody _   = Left "Multi-experssion bodies not yet supported"

readArgs :: [SExpression] -> RReader [String]
readArgs []     = Right []
readArgs (a:as) = do
  arg <- expectSymbol a
  rest <- readArgs as
  return $ arg:rest -- TODO: make sure args are unique

readExpression :: SExpression -> RReader RExpression
readExpression se = case sexprToValue se of
  Just plainValue -> Right $ RValue plainValue
  Nothing         ->
    case readSpecialForm se of
      Right expr -> Right expr
      Left _     -> readFnCall se

readSpecialForm :: SExpression -> RReader RExpression
readSpecialForm (SList items) = case items of
  (SSymbol "do":rest)     -> readDoForm rest
  (SSymbol "lambda":rest) -> readLambdaForm rest
  (SSymbol "let":rest)    -> readLetForm rest
  _                       -> Left "Not a special form"
readSpecialForm _ = Left "Not a special form"

readFnCall :: SExpression -> RReader RExpression
readFnCall = undefined

readDoForm :: [SExpression] -> RReader RExpression
readDoForm _ = Left "TODO: do form unimplemented"

readLambdaForm :: [SExpression] -> RReader RExpression
readLambdaForm _ = Left "TODO: lambda form unimplemented"

readLetForm :: [SExpression] -> RReader RExpression
readLetForm _ = Left "TODO: let form unimplemented"

expectSymbol :: SExpression -> RReader String
expectSymbol (SSymbol s) = Right s
expectSymbol x           = Left ("Expected a symbol, got " ++ (show x))

expectList :: SExpression -> RReader [SExpression]
expectList (SList l) = Right l
expectList x         = Left ("Expected a list, got " ++ (show x))

sexprToValue                  :: SExpression -> Maybe RPlainValue
sexprToValue (SList [])       = Just RNil
sexprToValue (SKey "nil")     = Just RNil
sexprToValue (SKey "true")    = Just RTrue
sexprToValue (SKey "t")       = Just RTrue
sexprToValue (SKey key)       = Just $ RKey key
sexprToValue (SSymbol sym)    = Just $ RSymbol sym
sexprToValue (SCharValue c)   = Just $ RChar c
sexprToValue (SStringValue s) = Just $ RString s
sexprToValue (SIntVal i)      = Just $ RInt i
sexprToValue (SFloatVal f)    = Just $ RFloat f
sexprToValue _                = Nothing
