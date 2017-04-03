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
  return ExitSuccess

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
    undefined
  (Block _ stmts) -> do
    let scopes' = Scope.new scopes
    undefined
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
  (Cast t e) ->
    error "TODO: cast"
  (Var _ name) -> case Scope.get scopes name of
    Left err  -> Left (show err)
    Right val -> return val
  (Access _ e s) ->
    error "TODO: access expression"
  (Lambda t a s) ->
    return $ LambdaVal t a s

applyUop :: UnaryOp -> IR.Value -> ValueResult
applyUop op val = error "TODO: applyUop"

applyBop :: BinOp -> IR.Value -> IR.Value -> ValueResult
applyBop op l r = error "TODO: applyBop"

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

stmtResult2Value :: StmtOutput -> IR.Value
stmtResult2Value (Continuing _) = IR.EmptyValue
stmtResult2Value (Returned val) = val

scopeErr2Result :: Either Scope.ScopeError a -> EvalResult a
scopeErr2Result = mapLeft show

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right r) = Right r
mapLeft f (Left l)  = Left (f l)
