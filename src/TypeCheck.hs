module TypeCheck where

import Control.Applicative ( (<|>) )
import Control.Monad (foldM)
import Control.Monad.State
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map

import AST.Declaration (Declaraction, File)
import qualified AST.Declaration as D
import AST.Expression (Expression, Value)
import qualified AST.Expression as E
import AST.Statement (Statement)
import qualified AST.Statement as S
import AST.Type (Type)
import qualified AST.Type as T

type Result = Either String
type TypeScope = [Map String Type]
type TSState = StateT TypeScope Result

checkFile :: File -> Result ()
checkFile file = evalStateT (checkFileM file) []

checkFileM :: File -> TSState ()
checkFileM file = do
  buildFileScope file
  _ <- mapM checkDeclaration file
  return ()

scopeLookup :: String -> TypeScope -> Maybe Type
scopeLookup name (m:ms) = Map.lookup name m <|> scopeLookup name ms
scopeLookup _    []     = Nothing

scopeAdd :: String -> Type -> TypeScope -> Result TypeScope
scopeAdd name typ (m:ms) =
  case Map.lookup name m of
   Nothing  -> return (Map.insert name typ m : ms)
   (Just x) -> Left ("Duplicate definition for: " ++ name)
scopeAdd _    _   []     = Left "Empty scope stack??"

startScope :: TSState ()
startScope = modify (Map.empty :)

endScope :: TSState ()
endScope = do
  scopes <- get
  case scopes of
   (_:ss) -> put ss
   []     -> lift $ Left "No scope to end??"

getFromScope :: String -> TSState Type
getFromScope name = do
  scopes <- get
  lift $ note ("Not defined: " ++ name) (scopeLookup name scopes)

setInScope :: String -> Type -> TSState ()
setInScope name typ = do
  scopes <- get
  newScopes <- lift $ scopeAdd name typ scopes
  put newScopes
  return ()

buildFileScope :: File -> TSState ()
buildFileScope file = do
  startScope
  _ <- mapM addDecl file
  return ()
  where addDecl decl = setInScope (declName decl) (declType decl)

declName :: Declaraction -> String
declName (D.Let n _ _) = n
declName (D.Function n _ _ _) = n

declType :: Declaraction  -> Type
declType (D.Let _ t _) = t
declType (D.Function _ t _ _) = t

addFuncScope :: [String] -> [Type] -> TSState ()
addFuncScope names types = do
  startScope
  _ <- mapM (\(n,t) -> setInScope n t) (zip names types)
  return ()

checkDeclaration :: Declaraction -> TSState ()
checkDeclaration d =
  case d of
   (D.Function _ t args body) -> do
     (argTypes, retType) <- case t of
       (T.Function ats rt) -> return (ats, rt)
       _                   -> lift $ Left $ "function with non-function type: " ++ show t
     if length argTypes /= length args
        then lift $ Left "arg length mismatch in declaration"
        else do
           addFuncScope args argTypes
           requireReturnType body retType
   (D.Let _ t expr) -> do
     _ <- requireExprType expr t
     return ()

requireReturnType :: Statement -> Type -> TSState ()
requireReturnType stmt t = do
  (lastRet, rets) <- getReturnType stmt
  _ <- mapM (lift . requireEqual t) rets
  lift $ requireEqual' t lastRet

getReturnType :: Statement -> TSState (Type, [Type])
getReturnType stmt =
  case stmt of
   (S.Block stmts) -> checkBlock stmts
   _               -> lift $ Left "function body must be a block"

checkBlock :: [Statement] -> TSState (Type, [Type])
checkBlock stmts =
  let localDeclarations = Map.empty
  in undefined

-- Returns a return type, if the statement returns
checkStatement :: TypeScope -> Statement -> TSState (Maybe Type)
checkStatement localScope s =
  case s of
   (S.Return mExpr) -> case mExpr of
     Nothing  -> return $ Just T.Nil
     (Just e) -> do
       retType <- checkExpression e
       return $ Just retType
   (S.Let name t e) -> do
     -- TODO: check local scope, add this to it
     _ <- requireExprType e t
     return Nothing
   (S.Assign name e) -> do
     -- TODO: require that name is bound, then check its type
     _ <- checkExpression e
     return Nothing
   (S.Block stmts) -> do
     -- TODO: walk over statements
     return Nothing
   (S.Expr e) -> do
     _ <- checkExpression e
     return Nothing
   (S.If test body mElse) -> do
     _ <- requireExprType test T.Bool
     -- todo: figure out how to handle this
     return Nothing
   (S.While test body) -> do
     _ <- requireExprType test T.Bool
     -- todo: figure out how to handle this
     return Nothing

requireExprType :: Expression -> Type -> TSState Type
requireExprType e t =
  case e of
   (E.EParen e')     -> requireExprType e' t
   (E.EValue v)      -> lift $ requireEqual t (valueType v)
   (E.EUnary _ e')   -> requireExprType e' t
   (E.EBinary _ l r) -> do
     _ <- requireExprType l t
     requireExprType r t
   (E.ECall f args)  -> do
     retType <- checkExpression e
     lift $ requireEqual t retType
   (E.ECast t' e')   -> do
     _ <- lift $ requireEqual t t'
     checkExpression e'
   (E.EVariable var) -> requireVarType var t

checkExpression :: Expression -> TSState Type
checkExpression e =
  case e of
   (E.EParen e')     -> checkExpression e'
   (E.EValue v)      -> return $ valueType v
   (E.EUnary o e')   -> checkExpression e'
   (E.EBinary o l r) -> do
     t <- checkExpression l
     requireExprType r t
   (E.ECall f args)  -> do
     fType <- checkExpression f
     case fType of
      (T.Function argTypes retType) ->
        if length argTypes /= length args
        then lift $ Left $ "arg length mismatch"
        else do
          _ <- mapM (\(t, arg) -> requireExprType arg t) (zip argTypes args)
          return retType
      _                   -> lift $ Left $ "trying to call a non-function type: " ++ show fType
   (E.ECast t' e')   -> do
     _ <- checkExpression e'
     return t'
   (E.EVariable var) -> getFromScope var

requireVarType :: String -> Type -> TSState Type
requireVarType var t = do
  t' <- getFromScope var
  if t == t'
    then return t
    else lift $ Left $ "type mismatch " ++ show t' ++ " and " ++ show t

valueType :: Value -> Type
valueType (E.EString _) = T.String
valueType (E.EBool _)   = T.Bool
valueType (E.EInt _)    = T.Int
valueType (E.EFloat _)  = T.Float

requireEqual :: Type -> Type -> Result Type
requireEqual t1 t2 =
  if t1 == t2 then return t1
  else Left $ "Type mismatch between " ++ show t1 ++ " and " ++ show t2

requireEqual' :: Type -> Type -> Result ()
requireEqual' t1 t2 = do
  _ <- requireEqual t1 t2
  return ()


note :: a -> Maybe b -> Either a b
note msg = maybe (Left msg) Right
