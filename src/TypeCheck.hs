module TypeCheck where

import Control.Applicative ( (<|>) )
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map

import AST.Declaration (Declaration, File)
import qualified AST.Declaration as D
import AST.Expression (Expression, Value)
import qualified AST.Expression as E
import AST.Statement (Statement)
import qualified AST.Statement as S
import AST.Type (Type)
import qualified AST.Type as T

type Result = Either String
-- Entries in a Scope have a double-meaning:
--  - lowercase names map to the type of the value
--  - uppercae names map to the type bound to that name
type Scope = Map String Type
type TypeScope = [Scope]
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

addDecl :: Declaration -> TSState ()
addDecl (D.Let n t _)        = setInScope n t
addDecl (D.Function n t _ _) = setInScope n t
addDecl (D.TypeDef n t)      =
  case t of
   (T.Enum options) -> do
     setInScope n t
     -- TODO: extend the system to both understand that these are subtypes of t,
     -- and to know that they are structure types
     _ <- mapM (\optName -> setInScope optName t) (map fst options)
     return ()
   _                -> do
     setInScope n t

defineDefaultFunctions :: TSState ()
defineDefaultFunctions = do
  setInScope "print" $ T.Function [T.String] T.Nil

addFuncScope :: [String] -> [Type] -> TSState ()
addFuncScope names types = do
  _ <- mapM (\(n,t) -> setInScope n t) (zip names types)
  return ()

checkDeclaration :: Declaration -> TSState ()
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
   (D.TypeDef _ _) -> return () -- already handled in buildFileScope

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
   (S.Assign names e) -> do
     t <- getAssignType names
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

getAssignType :: [String] -> TSState Type
getAssignType []           = err "compiler error: assign statement with no names"
getAssignType (var:fields) = do
  t <- getFromScope var
  foldM getFieldType t fields

requireExprType :: Expression -> Type -> TSState Type
requireExprType e t =
  case e of
   (E.Paren e')     -> do
     requireExprType e' t
   (E.Val v)        -> do
     vt <- valueType v
     requireEqual t vt
   (E.Unary op e')  -> do
     et <- checkExpression e'
     resultT <- unaryReturnType op et
     requireEqual t resultT
   (E.Binary o l r) -> do
     lt <- checkExpression l
     _ <- requireExprType r lt
     resultT <- binReturnType o lt
     _ <- requireEqual t resultT
     return resultT
   (E.Call f args)  -> do
     fnType <- checkExpression f
     argTypes <- mapM checkExpression args
     requireEqual fnType (T.Function argTypes t)
   (E.Cast t' e')   -> do
     _ <- requireEqual t t'
     checkExpression e'
   (E.Var var)      -> do
     requireVarType var t
   (E.Access e' f)  -> do
     et <- checkExpression e'
     t' <- getFieldType et f
     requireEqual t t'

checkExpression :: Expression -> TSState Type
checkExpression e =
  case e of
   (E.Paren e')     -> checkExpression e'
   (E.Val v)        -> valueType v
   (E.Unary o e')   -> do
     t <- checkExpression e'
     unaryReturnType o t
   (E.Binary o l r) -> do
     t <- checkExpression l
     _ <- requireExprType r t
     binReturnType o t
   (E.Call f args)  -> do
     fType <- checkExpression f
     case fType of
      (T.Function argTypes retType) ->
        if length argTypes /= length args
        then err "arg length mismatch"
        else do
          _ <- mapM (\(t, arg) -> requireExprType arg t) (zip argTypes args)
          return retType
      _  -> err $ "trying to call a non-function type: " ++ show fType
   (E.Cast t' e')   -> do
     _ <- checkExpression e'
     return t'
   (E.Var var)      -> do
     getFromScope var
   (E.Access e' f)  -> do
     et <- checkExpression e'
     getFieldType et f

getFieldType :: Type -> String -> TSState Type
getFieldType typ fieldName = do
  fieldTypes <- getStructFields typ
  case lookup fieldName fieldTypes of
   (Just t) -> return t
   Nothing  -> err $ "field " ++ fieldName ++ " not found on type " ++ show typ

getStructFields :: Type -> TSState [(String, Type)]
getStructFields typ =
  case typ of
   (T.Struct fields) -> return fields
   (T.TypeName name) -> do
     realType <- getFromScope name
     getStructFields realType
   _                 ->
     err $ "Can't access field  on a value of type " ++ show typ

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

valueType :: Value -> TSState Type
valueType (E.StrVal _)       = return T.String
valueType (E.BoolVal _)      = return T.Bool
valueType (E.IntVal _)       = return T.Int
valueType (E.FloatVal _)     = return T.Float
valueType (E.StructVal tn f) = do
  structType <- getFromScope tn
  _ <- mapM (checkField structType) f
  return structType

checkField :: Type -> (String, Expression) -> TSState ()
checkField typ (field, e) = do
  ftyp <- getFieldType typ field
  _ <- requireExprType e ftyp
  return ()

resolveTypeName :: Type -> TSState Type
resolveTypeName (T.TypeName name) = do
  t <- getFromScope name
  resolveTypeName t
resolveTypeName t                 =
  return t

requireEqual :: Type -> Type -> TSState Type
requireEqual t1 t2 = do
  t1' <- resolveTypeName t1
  t2' <- resolveTypeName t2
  if t1' == t2'
  then return t1'
  else err ("Type mismatch between " ++ show t1 ++ " and " ++ show t2)

note :: a -> Maybe b -> Either a b
note msg = maybe (Left msg) Right
