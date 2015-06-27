module Eval where

import Data.Bits (complement, (.&.), (.|.))
import Data.Map (Map)
import qualified Data.Map as Map

import Parse

data Closure = Closure EvalContext FunctionDef
             deriving (Eq, Show)

data Value = IntVal Int
           | FloatVal Float
           | StringVal String
           | StructVal String [Value]
           | FunctionVal Closure
           deriving (Eq, Show)

type EvalError = String
type EvalContext = Map String Value

type EvalResult = Either EvalError Value

orLeft :: Maybe a -> b -> Either b a
orLeft (Just a) _ = Right a
orLeft Nothing  b = Left b

litToVal :: LiteralValue -> Value
litToVal (LiteralInt    i) = IntVal    i
litToVal (LiteralFloat  f) = FloatVal  f
litToVal (LiteralString s) = StringVal s

evalExpression :: EvalContext -> Expression -> EvalResult
evalExpression _   (ExpressionLit literal)    =
  Right $ litToVal literal
evalExpression ctx (ExpressionVar varname)    =
  Map.lookup varname ctx `orLeft` ("Variable not found: " ++ varname)
evalExpression ctx (ExpressionParen inner)    =
  evalExpression ctx inner
evalExpression ctx (ExpressionFnCall fn args) =
  do
    argVals <- mapM (evalExpression ctx) args
    applyFn ctx fn argVals
evalExpression ctx (ExpressionBinary op l r)  =
  do
    left <- evalExpression ctx l
    right <- evalExpression ctx r
    evalBinary op left right
evalExpression ctx (ExpressionUnary op expr)  =
  do
    inner <- evalExpression ctx expr
    evalUnary op inner
evalExpression ctx (ExpressionStruct name vs) =
  do
    vals <- mapM (evalExpression ctx) vs
    return $ StructVal name vals

applyFn :: EvalContext -> FunctionName -> [Value] -> EvalResult
applyFn ctx name argVals = undefined -- TODO

evalUnary :: UnaryOp -> Value -> EvalResult
evalUnary op val = case val of
  (IntVal i)    ->
    case op of
     Negate -> Right . IntVal $ 0 - i
     Flip   -> Right . IntVal $ complement i
  (FloatVal f)  ->
    case op of
     Negate -> Right . FloatVal $ 0 - f
     _      -> Left $ "Can't apply unary op " ++ (display op) ++ " to a float"
  (StructVal _ _) ->
    case op of
     Not -> do
       boolVal <- ensureBool val
       return $ boolToStruct (not boolVal)
  _             -> Left $ "Can't apply unary op " ++ (display op)

evalBinary :: BinaryOp -> Value -> Value -> EvalResult
evalBinary op l r =
  case (l, r) of
   (IntVal il,    IntVal ir)    ->
     Right $ intOp op il ir
   (FloatVal fl,  FloatVal fr)  ->
     do
       opFn <- floatOp op
       return $ opFn fl fr
   (StringVal sl, StringVal sr) ->
     case op of
       Plus -> Right . StringVal $ sl ++ sr
       _    -> Left $ "Can't apply op " ++ (display op) ++ " to strings"
   (StructVal _ _, StructVal _ _) ->
     do
       opFn <- boolOp op
       opFn l r
   _ -> Left $ "Can't apply binary op " ++ (display op) ++ " to " ++ (show l) ++ " and " ++ (show r)

intOp :: BinaryOp -> Int -> Int -> Value
intOp Plus      = intValOp (+)
intOp Minus     = intValOp (-)
intOp Times     = intValOp (*)
intOp Divide    = intValOp div
intOp Mod       = intValOp mod
intOp Power     = intValOp (^)
intOp Less      = intBoolOp (<)
intOp LessEq    = intBoolOp (<=)
intOp Equals    = intBoolOp (==)
intOp Greater   = intBoolOp (>)
intOp GreaterEq = intBoolOp (>=)
intOp NotEq     = intBoolOp (/=)
intOp And       = intValOp (.&.)
intOp Or        = intValOp (.|.)

intValOp :: (Int -> Int -> Int) -> (Int -> Int -> Value)
intValOp op = (\l r -> IntVal (op l r))

intBoolOp :: (Int -> Int -> Bool) -> (Int -> Int -> Value)
intBoolOp op = (\l r -> boolToStruct $ op l r)

floatOp :: BinaryOp -> Either String (Float -> Float -> Value)
floatOp Plus      = floatValOp (+)
floatOp Minus     = floatValOp (-)
floatOp Times     = floatValOp (*)
floatOp Divide    = floatValOp (/)
floatOp Mod       = Left "Can't apply mod to floats"
floatOp Power     = floatValOp (**)
floatOp Less      = floatBoolOp (<)
floatOp LessEq    = floatBoolOp (<=)
floatOp Equals    = floatBoolOp (==)
floatOp Greater   = floatBoolOp (>)
floatOp GreaterEq = floatBoolOp (>=)
floatOp NotEq     = floatBoolOp (/=)
floatOp And       = Left "Can't apply and to floats"
floatOp Or        = Left "Can't apply op to floats"

floatValOp :: (Float -> Float -> Float) -> Either String (Float -> Float -> Value)
floatValOp op = return (\l r -> FloatVal (op l r))

floatBoolOp :: (Float -> Float -> Bool) -> Either String (Float -> Float -> Value)
floatBoolOp op = return (\l r -> boolToStruct $ op l r)

makeBoolOp :: (Bool -> Bool -> Bool) -> (Value -> Value -> EvalResult)
makeBoolOp fn = boolFn
  where boolFn left right = do
          lB <- ensureBool left
          rB <- ensureBool right
          return $ boolToStruct (fn lB rB)

boolOp :: BinaryOp -> Either EvalError (Value -> Value -> EvalResult)
boolOp Equals = Right $ makeBoolOp (==)
boolOp NotEq  = Right $ makeBoolOp (/=)
boolOp And    = Right $ makeBoolOp (&&)
boolOp Or     = Right $ makeBoolOp (||)
boolOp op     = Left $ "Can't apply " ++ display op ++ " to booleans"

ensureBool :: Value -> Either EvalError Bool
ensureBool (StructVal "True"  []) = Right True
ensureBool (StructVal "False" []) = Right False
ensureBool expr                   = Left $ ("type error: " ++ show expr ++ " isn't boolean")

boolToStruct :: Bool -> Value
boolToStruct b = StructVal (if b then "True" else "False") []
