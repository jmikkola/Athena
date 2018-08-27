module Interpreter (interpret) where


import System.IO (hFlush, stdout)

import Data.Map (Map)
import qualified Data.Map as Map

import Inference
  ( InferResult(..)
  , topLevelBindings )
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import Types (Type(..), tUnit)


interpret :: InferResult () -> IO ()
interpret body =
  let mainT = TFunc [] tUnit
      callMain = E.Call (tUnit, ()) (E.Var (mainT, ()) "main") []
  in do
    let scope = startingState body
    _ <- interpretExpr scope callMain
    return ()


startingState :: InferResult () -> Scope
startingState body =
  let decls = topLevelBindings body
      -- TODO: Also evaluate constant expression
      bindings =
        [ (name, toClosure scope args stmt)
        | (name, D.Function _ _ _ args stmt) <- decls ]
      scope = [Map.fromList bindings]
  in scope

interpretExpr :: Scope -> E.Expression AnnT -> IO Value
interpretExpr scope expr = case expr of
  E.Paren _ ex ->
    interpretExpr scope ex
  E.Val _ val ->
    interpretVal scope val
  E.Unary _ uop ex -> do
    val <- interpretExpr scope ex
    applyUOp uop val
  E.Binary _ bop l r -> do
    lVal <- interpretExpr scope l
    rVal <- interpretExpr scope r
    applyBOp bop lVal rVal
  E.Call _ fnEx argExs -> do
    fnVal <- interpretExpr scope fnEx
    argVals <- mapM (interpretExpr scope) argExs
    callFunction fnVal argVals
  E.Cast _ t ex -> do
    val <- interpretExpr scope ex
    castVal t val
  E.Var _ name -> do
    lookupVar scope name
  E.Access _ ex field -> do
    val <- interpretExpr scope ex
    accessField val field

data StmtResult
  = Returned Value
  | FellThrough

getReturnValue :: StmtResult -> Value
getReturnValue (Returned val) = val
getReturnValue FellThrough = VVoid

interpretStmt :: Scope -> S.Statement AnnT -> IO StmtResult
interpretStmt scope stmt = case stmt of
  S.Return _ Nothing -> do
    return $ Returned VVoid
  S.Return _ (Just expr) -> do
    val <- interpretExpr scope expr
    return $ Returned VVoid
  S.Let _ name _ expr -> do
    val <- interpretExpr scope expr
    error "TODO: Let"
    return $ FellThrough
  S.Assign _ names expr -> do
    val <- interpretExpr scope expr
    error "TODO: Assign"
    return $ FellThrough
  S.Block _ stmts ->
    interpretBlock scope stmts
  S.Expr _ expr -> do
    _ <- interpretExpr scope expr
    return $ FellThrough
  S.If _ tst thn els -> do
    testVal <- interpretExpr scope tst
    error "TODO: If"
    return $ FellThrough
  S.While _ tst blk -> do
    testVal <- interpretExpr scope tst
    error "TODO: While"
    return $ FellThrough


interpretBlock :: Scope -> [S.Statement AnnT] -> IO StmtResult
interpretBlock scope [] =
  return FellThrough
interpretBlock scope (s:stmts) = do
  result <- interpretStmt scope s
  case result of
   FellThrough -> interpretBlock scope stmts
   Returned _  -> return result


interpretVal :: Scope -> E.Value AnnT -> IO Value
interpretVal scope val = case val of
  E.StrVal _ s              -> return $ VString s
  E.BoolVal _ b             -> return $ VBool b
  E.IntVal _ i              -> return $ VInt i
  E.FloatVal _ f            -> return $ VFloat f
  E.StructVal _ name fields -> do
    let mapField (fname, fexpr) = do
          fval <- interpretExpr scope fexpr
          return (fname, fval)
    vals <- mapM mapField fields
    return $ VStruct name vals


applyUOp :: E.UnaryOp -> Value -> IO Value
applyUOp = error "TODO: applyUOp"

applyBOp :: E.BinOp -> Value -> Value -> IO Value
applyBOp = error "TODO: applyBOp"

callFunction :: Value -> [Value] -> IO Value
callFunction (VClosure scope (Function names body)) args = do
  let fnScope = (Map.fromList $ zip names args) : scope
  result <- interpretStmt fnScope body
  return $ getReturnValue result

castVal :: String -> Value -> IO Value
castVal t val = error "TODO: castVal"

lookupVar :: Scope -> String -> IO Value
lookupVar scope name =
  return $ head [val | Just val <- map (Map.lookup name) scope]

accessField :: Value -> String -> IO Value
accessField = error "TODO: accessField"

data Value
  = VInt Int
  | VFloat Float
  | VString String
  | VBool Bool
  | VStruct String [(String, Value)]
  | VList [Value]
  | VClosure Scope Function
  | VVoid
  deriving (Show)

data Function
  = Function [String] (S.Statement AnnT)
  deriving (Show)

type Scope = [Map String Value]

-- AnnT is short for "Annotation Type"
type AnnT = (Type, ())


toClosure :: Scope -> [String] -> S.Statement AnnT -> Value
toClosure scope args stmt =
  VClosure scope $ Function args stmt
