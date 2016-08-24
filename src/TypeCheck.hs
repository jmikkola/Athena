module TypeCheck where

import Control.Applicative ( (<|>) )
import Control.Monad.State
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
   (Just _) -> Left ("Duplicate definition for: " ++ name)
scopeAdd _    _   []     = Left "Empty scope stack??"

startScope :: TSState ()
startScope = modify (Map.empty :)

endScope :: TSState ()
endScope = do
  scopes <- get
  case scopes of
   (_:ss) -> put ss
   []     -> err "No scope to end??"

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
  defineDefaultFunctions
  _ <- mapM addDecl file
  return ()
  where addDecl decl = setInScope (declName decl) (declType decl)

defineDefaultFunctions :: TSState ()
defineDefaultFunctions = do
  setInScope "print" $ T.Function [T.String] T.Nil

declName :: Declaraction -> String
declName (D.Let n _ _) = n
declName (D.Function n _ _ _) = n
declName (D.TypeDef n _) = n

declType :: Declaraction  -> Type
declType (D.Let _ t _) = t
declType (D.Function _ t _ _) = t
declType (D.TypeDef _ _) = T.Nil -- no reflection just yet

addFuncScope :: [String] -> [Type] -> TSState ()
addFuncScope names types = do
  _ <- mapM (\(n,t) -> setInScope n t) (zip names types)
  return ()

checkDeclaration :: Declaraction -> TSState ()
checkDeclaration d =
  case d of
   (D.Function _ t args body) -> do
     (argTypes, retType) <- case t of
       (T.Function ats rt) -> return (ats, rt)
       _                   -> err $ "function with non-function type: " ++ show t
     if length argTypes /= length args
        then err "arg length mismatch in declaration"
        else do
           startScope
           addFuncScope args argTypes
           requireReturnType body retType
           endScope
   (D.Let _ t expr) -> do
     _ <- requireExprType expr t
     return ()
   (D.TypeDef _ _) -> return () -- TODO: keep some map of known types

requireReturnType :: Statement -> Type -> TSState ()
requireReturnType stmt t = do
  lastRet <- getReturnType stmt t
  _ <- requireEqual t lastRet
  return ()

getReturnType :: Statement -> Type -> TSState Type
getReturnType stmt expectedRetType =
  case stmt of
   (S.Block stmts) -> checkBlock stmts expectedRetType
   _               -> err "function body must be a block"

err :: String -> StateT TypeScope (Either String) a
err = lift . Left

checkBlock :: [Statement] -> Type -> TSState Type
checkBlock stmts expectedRetType =
  case stmts of
   []               -> return T.Nil
   [S.Return me] -> returnExpr me expectedRetType
   (S.Return _:_)   -> err $ "Statements after a return: " ++ show stmts
   (s:ss)           -> do
     _ <- checkStatement s expectedRetType
     checkBlock ss expectedRetType

returnExpr :: Maybe Expression -> Type -> TSState Type
returnExpr Nothing  expectedRetType = do
  requireEqual expectedRetType T.Nil
returnExpr (Just e) expectedRetType = do
  retType <- checkExpression e
  requireEqual expectedRetType retType

-- Returns a return type, if the statement returns
checkStatement :: Statement -> Type -> TSState ()
checkStatement s expectedRetType =
  case s of
   (S.Return mExpr) -> do
     _ <- returnExpr mExpr expectedRetType
     return ()
   (S.Let name t e) -> do
     setInScope name t
     _ <- requireExprType e t
     return ()
   (S.Assign name e) -> do
     t <- getFromScope name
     _ <- requireExprType e t
     return ()
   (S.Block stmts) -> do
     startScope
     _ <- checkBlock stmts expectedRetType
     endScope
   (S.Expr e) -> do
     _ <- checkExpression e
     return ()
   (S.If test body mElse) -> do
     _ <- requireExprType test T.Bool

     startScope
     _ <- checkBlock body expectedRetType
     endScope

     _ <- case mElse of
       Nothing    -> return ()
       (Just els) -> checkStatement els expectedRetType

     return ()
   (S.While test body) -> do
     _ <- requireExprType test T.Bool

     startScope
     _ <- checkBlock body expectedRetType
     endScope

     return ()

requireExprType :: Expression -> Type -> TSState Type
requireExprType e t =
  case e of
   (E.EParen e')     -> requireExprType e' t
   (E.EValue v)      -> requireEqual t (valueType v)
   (E.EUnary op e')  -> do
     et <- checkExpression e'
     resultT <- unaryReturnType op et
     requireEqual t resultT
   (E.EBinary o l r) -> do
     lt <- checkExpression l
     _ <- requireExprType r lt
     resultT <- binReturnType o lt
     _ <- requireEqual t resultT
     return resultT
   (E.ECall f args)  -> do
     fnType <- checkExpression f
     argTypes <- mapM checkExpression args
     requireEqual fnType (T.Function argTypes t)
   (E.ECast t' e')   -> do
     _ <- requireEqual t t'
     checkExpression e'
   (E.EVariable var) -> requireVarType var t

checkExpression :: Expression -> TSState Type
checkExpression e =
  case e of
   (E.EParen e')     -> checkExpression e'
   (E.EValue v)      -> return $ valueType v
   (E.EUnary o e')   -> do
     t <- checkExpression e'
     unaryReturnType o t
   (E.EBinary o l r) -> do
     t <- checkExpression l
     _ <- requireExprType r t
     binReturnType o t
   (E.ECall f args)  -> do
     fType <- checkExpression f
     case fType of
      (T.Function argTypes retType) ->
        if length argTypes /= length args
        then err "arg length mismatch"
        else do
          _ <- mapM (\(t, arg) -> requireExprType arg t) (zip argTypes args)
          return retType
      _  -> err $ "trying to call a non-function type: " ++ show fType
   (E.ECast t' e')   -> do
     _ <- checkExpression e'
     return t'
   (E.EVariable var) -> getFromScope var

unaryReturnType :: E.UnaryOp -> Type -> TSState Type
unaryReturnType op t =
  case op of
   E.BitInvert -> requireEqual t T.Int
   E.BoolNot   -> requireEqual t T.Bool

numericOps :: [E.BinOp]
numericOps = [E.Minus, E.Times, E.Divide, E.Power]

integerOps :: [E.BinOp]
integerOps = [E.Mod, E.BitAnd, E.BitOr, E.BitXor, E.LShift, E.RShift, E.RRShift]

booleanOps :: [E.BinOp]
booleanOps = [E.BoolAnd, E.BoolOr]

compOps    :: [E.BinOp]
compOps    = [E.Eq, E.NotEq]

numCompOps :: [E.BinOp]
numCompOps = [E.Less, E.LessEq, E.Greater, E.GreaterEq]

binReturnType :: E.BinOp -> Type -> TSState Type
binReturnType op t
  | op == E.Plus          =
    if t `elem` [T.Int, T.Float, T.String]
    then return t
    else err $ "Can't add values of type: " ++ show t
  | op `elem` numericOps = requireNumeric t
  | op `elem` integerOps = requireEqual t T.Int
  | op `elem` booleanOps = requireEqual t T.Bool
  | op `elem` compOps    = return T.Bool
  | op `elem` numCompOps = do
      _ <- requireNumeric t
      return T.Bool
  | otherwise            = err $ "op missing from list: " ++ show op

requireNumeric :: Type -> TSState Type
requireNumeric t =
  case t of
   T.Int   -> return t
   T.Float -> return t
   _       -> err $ "Expected a numeric value: " ++ show t

requireVarType :: String -> Type -> TSState Type
requireVarType var t = do
  t' <- getFromScope var
  if t == t'
    then return t
    else err $ "type mismatch " ++ show t' ++ " and " ++ show t

valueType :: Value -> Type
valueType (E.EString _)    = T.String
valueType (E.EBool _)      = T.Bool
valueType (E.EInt _)       = T.Int
valueType (E.EFloat _)     = T.Float
valueType (E.EStruct tn _) = T.TypeName tn

requireEqual :: Type -> Type -> TSState Type
requireEqual t1 t2 =
  if t1 == t2 then return t1
  else err ("Type mismatch between " ++ show t1 ++ " and " ++ show t2)

note :: a -> Maybe b -> Either a b
note msg = maybe (Left msg) Right
