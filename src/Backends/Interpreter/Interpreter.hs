module Backends.Interpreter.Interpreter where

import System.Exit ( ExitCode(..) )
import Data.Map (Map)

import AST.Expression
  ( UnaryOp (..)
  , BinOp (..)
  )
import IR
import Type (Type, TypeRef)
import qualified Backends.Interpreter.Scope as Scope

type Scopes = Scope.Scopes IR.Value

type EvalError = String
type EvalResult = Either EvalError
type ValueResult = EvalResult IR.Value

data StmtOutput
  = Returned IR.Value
  | Continuing Scopes
  deriving (Show)

type StmtResult = EvalResult StmtOutput

run :: Map TypeRef Type -> [IR.Decl] -> IO ExitCode
run types file = do
  putStrLn "todo"
  case runResult types file of
   Left err -> do
     putStrLn $ "error: " ++ show err
     return $ ExitFailure 1
   Right _ ->
     return ExitSuccess

runResult types file = do
  scopes <- buildRootScope file
  main <- scopeErr2Result $ Scope.get scopes "main"
  evalExpr scopes (IR.Call mainT (lambda2expr main) [])

lambda2expr :: IR.Value -> IR.Expression
lambda2expr (IR.LambdaVal t args stmt) = IR.Lambda t args stmt
lambda2expr v = error $ "Compiler error: not a lambda value: " ++ show v

mainT = "FN" -- TODO

buildRootScope :: [IR.Decl] -> EvalResult Scopes
buildRootScope decls =
  let bindings = getBindings decls
  in scopeErr2Result $ Scope.declareAll Scope.empty bindings

getBindings :: [IR.Decl] -> [(String, IR.Value)]
getBindings decls = f decls []
  where f dcs rest = case dcs of
          [] -> rest
          (TypeDecl _ _:xs) -> f xs rest
          (StmtDecl st :xs) -> f xs (getBinding st : rest)

getBinding :: IR.Statement -> (String, IR.Value)
getBinding (Let name _ (Lambda t args body)) =
  (name, IR.LambdaVal t args body)
getBinding decl =
  error $ "Compiler error: non-function top-level binding: " ++ show decl

evalStmt :: Scopes -> IR.Statement -> StmtResult
evalStmt scopes stmt = case stmt of
  (Return mExp) -> case mExp of
    Nothing -> return $ Returned IR.EmptyValue
    Just e -> do
      v <- evalExpr scopes e
      return $ Returned v
  (Let name _ expr) -> do
    v <- evalExpr scopes expr
    scopes' <- scopeErr2Result $ Scope.declare scopes name v
    return $ Continuing scopes'
  (Assign names expr) -> do
    v <- evalExpr scopes expr
    updateNames scopes names v
  (Block _ stmts) ->
    evalBlock (Scope.new scopes) stmts
  (Expr e) -> do
    _ <- evalExpr scopes e
    return $ Continuing scopes
  (If e th mel) -> do
    testVal <- evalExpr scopes e
    if testVal == IR.BoolVal True
      then evalStmt scopes th
      else case mel of
            Nothing -> return $ Continuing scopes
            Just el -> evalStmt scopes el
  (While e stmt) -> do
    testVal <- evalExpr scopes e
    if testVal == IR.BoolVal True
      then do
        stmtResult <- evalStmt scopes stmt
        case stmtResult of
          Continuing scopes' ->
            evalStmt scopes' stmt -- loop
          _ ->
            return stmtResult
      else return $ Continuing scopes
  (Match expr cases) -> do
    testVal <- evalExpr scopes expr
    Left "TODO: Match expressions"

updateNames :: Scopes -> [String] -> IR.Value -> StmtResult
updateNames scopes names value = do
  existingRoot <- scopeGet scopes (head names)
  newRoot <- updateField (tail names) existingRoot value
  newScopes <- scopeUpdate scopes (head names) newRoot
  return $ Continuing newScopes

updateField :: [String] -> IR.Value -> IR.Value -> ValueResult
updateField []     _    val = return val
updateField (f:fs) root val = case root of
  (StructVal t fields) -> case lookup f fields of
    Nothing -> Left $ "Compiler error: missing field name " ++ f
    Just (IR.Val ch) -> do
      ch' <- updateField fs ch val
      return $ StructVal t (setField f (IR.Val ch') fields)
    _ -> error "Compiler bug: can't walk non-value expression"
  _ -> error "Compiler bug in updateField"

setField :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
setField key value mapping = case mapping of
  ((a, b):rs) -> if key == a then (key, value) : rs
                 else (a, b) : setField key value rs
  []          -> [(key, value)]

evalBlock :: Scopes -> [IR.Statement] -> StmtResult
evalBlock scopes [] =
  return $ Continuing scopes
evalBlock scopes (st:sts) = do
  stResult <- evalStmt scopes st
  case stResult of
   Continuing scopes' -> evalBlock scopes' sts
   Returned val       -> return $ Returned val

evalExpr :: Scopes -> IR.Expression -> ValueResult
evalExpr scopes expr = case expr of
  (Paren e _) ->
    evalExpr scopes e
  (Val v) ->
    return v
  (Unary _ uop e) -> do
    val <- evalExpr scopes e
    applyUop uop val
  (Binary _ bop l r) -> do
    lval <- evalExpr scopes l
    rval <- evalExpr scopes r
    applyBop bop lval rval
  (Call _ fexp argExps) -> do
    lambda <- evalExpr scopes fexp
    argVals <- mapM (evalExpr scopes) argExps
    applyLambda scopes lambda argVals
  (Cast t e) -> do
    v <- evalExpr scopes e
    cast v t
  (Var _ name) -> case Scope.get scopes name of
    Left err  -> Left (show err)
    Right val -> return val
  (Access _ e field) -> do
    v <- evalExpr scopes e
    fieldExpr <- accessField v field
    evalExpr scopes fieldExpr
  (Lambda t a s) ->
    return $ LambdaVal t a s

applyUop :: UnaryOp -> IR.Value -> ValueResult
applyUop op val = case val of
  (IR.IntVal i) -> do
    result <- doNumUop op i
    return $ IR.IntVal result
  (IR.FloatVal f) -> do
    result <- doNumUop op f
    return $ IR.FloatVal result
  (IR.BoolVal b) -> do
    result <- doBoolUop op b
    return $ IR.BoolVal result
  _ -> error "Compiler bug: non number for uop"

applyBop :: BinOp -> IR.Value -> IR.Value -> ValueResult
applyBop Eq l r = undefined -- TODO
applyBop op l r = case (l, r) of
  (IR.IntVal li, IR.IntVal ri) -> do
    result <- doNumBop op li ri
    return $ IR.IntVal result
  (IR.FloatVal lf, IR.FloatVal rf) -> do
    result <- doNumBop op lf rf
    return $ IR.FloatVal result
  (IR.Bool lb, IR.Bool rb) -> do
    result <- doBoolBop op l r
    return $ IR.Bool result
  _ ->
    error "Compiler bug: wrong types for applyBop"

doNumUop :: (Num n) -> UnaryOp -> n -> EvalResult n
doNumUop BitInvert _ = error "TODO: bit invert"
doNumUop BoolNot   _ = Left "compiler bug: can't apply BoolNot to number"

doBoolUop :: UnaryOp -> Bool -> EvalResult Bool
doBoolUop BitInvert _ = Left "compiler bug: can't apply BitInvert to bool"
doBoolUop BoolNot   b = return $ not b

doNumBop :: (Num n) => BinaryOp -> n -> n -> EvalResult n
doNumBop op l r = undefined

doBoolBop :: BinaryOp -> Bool -> Bool -> EvalResult Bool


applyLambda :: Scopes -> IR.Value -> [IR.Value] -> ValueResult
applyLambda scopes lambda argValues = case lambda of
  (LambdaVal _ argNames stmt) ->
    if length argNames /= length argValues
    then error "Compiler bug: arg number doesn't match"
    else do
      let declarations = zip argNames argValues
      fnScope <- scopeErr2Result $ Scope.introduce scopes declarations
      stmtResult <- evalStmt fnScope stmt
      return $ stmtResult2Value stmtResult
  _ -> error $ "Compiler bug: expected a lambda, got " ++ show lambda

cast :: IR.Value -> TypeRef -> ValueResult
cast val t = case t of
  "String" ->
    return $ IR.StrVal $ show val
  "Int" -> case val of
    IR.IntVal i ->
      return $ IR.IntVal i
    IR.FloatVal f ->
      return $ IR.IntVal $ floor f
    _ ->
      error $ "Compiler bug: can't cast to Int: " ++ show val
  "Float" -> case val of
    IR.IntVal i ->
      return $ IR.FloatVal $ fromIntegral i
    IR.FloatVal f ->
      return $ IR.FloatVal f
    _ ->
      error $ "Compiler bug: can't cast to Int: " ++ show val
  _ ->
    error $ "Compiler bug: can't cast to " ++ show t

accessField :: IR.Value -> String -> EvalResult IR.Expression
accessField val name = case val of
  (IR.StructVal _ fields) -> case lookup name fields of
    Nothing -> error "Compiler bug in accessField: missing field name"
    Just ex -> return ex
  _ -> error "Compiler bug in accessField: not a struct"

stmtResult2Value :: StmtOutput -> IR.Value
stmtResult2Value (Continuing _) = IR.EmptyValue
stmtResult2Value (Returned val) = val

scopeErr2Result :: Either Scope.ScopeError a -> EvalResult a
scopeErr2Result = mapLeft show

scopeGet :: (Scope.Scopes a) -> String -> EvalResult a
scopeGet s n = scopeErr2Result $ Scope.get s n

scopeUpdate :: (Scope.Scopes a) -> String -> a -> EvalResult (Scope.Scopes a)
scopeUpdate s n v = scopeErr2Result $ Scope.update s n v

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right r) = Right r
mapLeft f (Left l)  = Left (f l)
