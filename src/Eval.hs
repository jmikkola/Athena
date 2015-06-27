module Eval where

import Data.Bits (complement, (.&.), (.|.))
import Data.Map (Map)
import qualified Data.Map as Map

import Parse

data Closure = Closure EvalContext FunctionDef
             deriving (Eq, Show)

data Value = IntVal Int
           | FloatVal Float
           | CharVal Char
           | StringVal String
           | StructVal String [Value]
           | FunctionVal Closure
           | NilValue -- Because we don't have typechecking yet
           deriving (Eq, Show)

type EvalError = String

type EvalResult = Either EvalError Value

type ScopeVars = Map String Value

data EvalContext = EvalContext ScopeVars | NestedContext ScopeVars EvalContext
                 deriving (Eq, Show)

emptyContext :: EvalContext
emptyContext = EvalContext Map.empty

newScope :: EvalContext -> EvalContext
newScope ctx = NestedContext Map.empty ctx

getScope :: EvalContext -> ScopeVars
getScope (EvalContext   scopeVars)   = scopeVars
getScope (NestedContext scopeVars _) = scopeVars

getParent :: EvalContext -> EvalContext
getParent (NestedContext _ parent) = parent
getParent ctx                      = error ("Error in interpreter: trying to get parent of " ++ show ctx)

getInScope :: String -> ScopeVars -> EvalResult
getInScope name scopeVars = case Map.lookup name scopeVars of
  Nothing  -> Left $ "Variable not found: " ++ name
  Just val -> Right val

getVar :: String -> EvalContext -> EvalResult
getVar name ctx = case ctx of
  (EvalContext   scopeVars)        -> getInScope name scopeVars
  (NestedContext scopeVars parent) -> case getInScope name scopeVars of
    Right val -> Right val
    Left  _   -> getVar name parent

insertNewVar :: String -> Value -> ScopeVars -> Either EvalError ScopeVars
insertNewVar name value scopeVars = case Map.lookup name scopeVars of
  Just _  -> Left $ "Variable already declared in scope: " ++ name
  _       -> return $ Map.insert name value scopeVars

letVar :: String -> Value -> EvalContext -> Either EvalError EvalContext
letVar name value ctx = case ctx of
  (EvalContext   scopeVars)        -> do
    scopeVars' <- insertNewVar name value scopeVars
    return $ EvalContext scopeVars'
  (NestedContext scopeVars parent) -> do
    scopeVars' <- insertNewVar name value scopeVars
    return $ NestedContext scopeVars' parent

setExisting :: String -> Value -> ScopeVars -> Either EvalError ScopeVars
setExisting name value scopeVars = case Map.lookup name scopeVars of
  Just _  -> return $ Map.insert name value scopeVars
  _       -> Left $ name ++ " not defined"

updateVar :: String -> Value -> EvalContext -> Either EvalError EvalContext
updateVar name value ctx = case ctx of
  (EvalContext   scopeVars)        -> do
    scopeVars' <- setExisting name value scopeVars
    return $ EvalContext scopeVars'
  (NestedContext scopeVars parent) -> case setExisting name value scopeVars of
    Right scopeVars' -> Right $ NestedContext scopeVars' parent
    Left  err        -> do
      parent' <- updateVar name value parent
      return $ NestedContext scopeVars parent'

orLeft :: Maybe a -> b -> Either b a
orLeft (Just a) _ = Right a
orLeft Nothing  b = Left b

evalStatement :: EvalContext -> Statement -> Either EvalError (EvalContext, Value)
evalStatement ctx (StatementExpr expr) = do
  result <- evalExpression ctx expr
  return (ctx, result)
evalStatement ctx (StatementLet var expr) = do
  value <- evalExpression ctx expr
  ctx' <- letVar var value ctx
  return (ctx', value)
evalStatement ctx (StatementAssign var expr) = do
  value <- evalExpression ctx expr
  ctx' <- updateVar var value ctx
  return (ctx', value)
evalStatement ctx (StatementReturn expr) = do
  result <- evalExpression ctx expr
  return (ctx, result)
evalStatement ctx (StatementIf test blk elPart) = evalIf ctx test blk elPart
evalStatement ctx (StatementWhile test blk) = evalWhile ctx test blk
evalStatement ctx (StatementFn funcDef) =
  let (FunctionDef name args _ body) = funcDef
      value = FunctionVal (Closure ctx funcDef)
  in do
    -- TODO: walk the function definition to find which variables are closed over; look for returns
    ctx' <- letVar name value ctx
    return (ctx', value)

-- for loops don't make sense yet because there is no iterable type
evalStatement _ st = Left $ "Evaluation not implemented for " ++ show st

evalElsePart :: EvalContext -> ElsePart -> Either EvalError (EvalContext, Value)
evalElsePart ctx elPart = case elPart of
  NoElse                    -> Right (ctx, NilValue)
  (Else        blk)         -> evalBlock ctx blk
  (ElseIf test blk elPart') -> evalIf ctx test blk elPart'

evalIf :: EvalContext -> Expression -> Block -> ElsePart -> Either EvalError (EvalContext, Value)
evalIf ctx test blk elPart = do
  testResult <- evalExpression ctx test
  asBool <- ensureBool testResult
  if asBool then evalBlock ctx blk
    else evalElsePart ctx elPart

evalWhile :: EvalContext -> Expression -> Block -> Either EvalError (EvalContext, Value)
evalWhile ctx test blk = do
  testResult <- evalExpression ctx test
  asBool <- ensureBool testResult
  if not asBool
     then return (ctx, NilValue)
     else do
      (ctx', _) <- evalBlock ctx blk
      evalWhile ctx' test blk

evalBlock :: EvalContext -> Block -> Either EvalError (EvalContext, Value)
evalBlock ctx (Block stmts) = do
  (ctx', value) <- evalStmts (newScope ctx) stmts
  return (getParent ctx', value)

evalStmts :: EvalContext -> [Statement] -> Either EvalError (EvalContext, Value)
evalStmts ctx []     = Right (ctx, NilValue)
evalStmts ctx [stmt] = evalStatement ctx stmt
evalStmts ctx (s:ss) = do
  (ctx', _) <- evalStatement ctx s
  evalStmts ctx' ss

litToVal :: LiteralValue -> Value
litToVal (LiteralInt    i) = IntVal    i
litToVal (LiteralFloat  f) = FloatVal  f
litToVal (LiteralString s) = StringVal s
litToVal (LiteralChar   c) = CharVal   c

evalExpression :: EvalContext -> Expression -> EvalResult
evalExpression _   (ExpressionLit lit)    =
  Right $ litToVal lit
evalExpression ctx (ExpressionVar varname)    =
  getVar varname ctx
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
applyFn ctx fname argValues = do
  FunctionVal (Closure cCtx (FunctionDef _ args _ body)) <- getVar fname ctx
  fCtx <- makeFnCtx argValues (map argName args) cCtx
  (_, value) <- evalBlock fCtx body
  return value

makeFnCtx :: [Value] -> [String] -> EvalContext -> Either EvalError EvalContext
makeFnCtx values args ctx = if length values /= length args
                            then Left ( "Number of supplied arguments doesn't match. "
                                        ++ " Expected values for " ++ show args
                                        ++ ", but got " ++ show values)
                            else Right $ NestedContext (Map.fromList $ zip args values) ctx

evalUnary :: UnaryOp -> Value -> EvalResult
evalUnary op val = case val of
  (IntVal i)    ->
    case op of
     Negate -> Right . IntVal $ 0 - i
     Flip   -> Right . IntVal $ complement i
     _      -> Left $ "Can't apply unary op " ++ (display op) ++ " to an int"
  (FloatVal f)  ->
    case op of
     Negate -> Right . FloatVal $ 0 - f
     _      -> Left $ "Can't apply unary op " ++ (display op) ++ " to a float"
  (StructVal _ _) ->
    case op of
     Not -> do
       boolVal <- ensureBool val
       return $ boolToStruct (not boolVal)
     _      -> Left $ "Can't apply unary op " ++ (display op) ++ " to a structure"
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
