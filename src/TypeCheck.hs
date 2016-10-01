module TypeCheck where

import Control.Applicative ( (<|>) )
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import IR
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T
import Type

type TypeName = String

type Result = Either String
-- Entries in a Scope have a double-meaning:
--  - lowercase names map to the type of the value
--  - uppercae names map to the type bound to that name
type Scope = Map String Type
type TypeScope = [Scope]
type Subtypes = Map TypeName (Set TypeName)
type TSState = StateT (TypeScope, Subtypes) Result

-- Monad functions --

getTypeScope :: TSState TypeScope
getTypeScope = liftM fst $ get

getSubtypes :: TSState Subtypes
getSubtypes = liftM snd $ get

putTypeScope :: TypeScope -> TSState ()
putTypeScope ts = do
  subs <- getSubtypes
  put (ts, subs)

putSubtypes :: Subtypes -> TSState ()
putSubtypes subs = do
  ts <- getTypeScope
  put (ts, subs)

updateTypeScope :: (TypeScope -> TypeScope) -> TSState ()
updateTypeScope f = do
  ts <- getTypeScope
  putTypeScope (f ts)

updateSubtypes :: (Subtypes -> Subtypes) -> TSState ()
updateSubtypes f = do
  subs <- getSubtypes
  putSubtypes (f subs)

-- Scaffolding functions --

err :: String -> TSState a
err = lift . Left

note :: a -> Maybe b -> Either a b
note msg = maybe (Left msg) Right

beginScope :: TSState ()
beginScope = updateTypeScope (Map.empty :)

endScope :: TSState ()
endScope = do
  ts <- getTypeScope
  case ts of
   []     -> err "compiler bug: ending no scopes"
   (_:ss) -> putTypeScope ss

scopeLookup :: String -> TypeScope -> Maybe Type
scopeLookup name (m:ms) = Map.lookup name m <|> scopeLookup name ms
scopeLookup _    []     = Nothing

getFromScope :: String -> TSState Type
getFromScope name = do
  ts <- getTypeScope
  lift $ note ("Not defined: " ++ name) (scopeLookup name ts)

scopeAdd :: String -> Type -> TypeScope -> Result TypeScope
scopeAdd name typ (m:ms) =
  case Map.lookup name m of
   Nothing  -> return (Map.insert name typ m : ms)
   (Just _) -> Left ("Duplicate definition for: " ++ name)
scopeAdd _    _   []     = Left "Empty scope stack??"

setInScope :: String -> Type -> TSState ()
setInScope name typ = do
  ts <- getTypeScope
  newScopes <- lift $ scopeAdd name typ ts
  putTypeScope newScopes

addSubtype :: TypeName -> TypeName -> TSState ()
addSubtype sub super = updateSubtypes addSub
  where addSub subtypes =
          let newSupers = case Map.lookup sub subtypes of
                Nothing -> Set.singleton super
                (Just sups) -> Set.insert super sups
          in Map.insert sub newSupers subtypes

getSuperTypesOf :: TypeName -> TSState (Set TypeName)
getSuperTypesOf sub = do
  subs <- getSubtypes
  case Map.lookup sub subs of
   Nothing -> return Set.empty
   Just st -> return st

-- Typing functions --

runTypechecking :: D.File -> Result [Decl]
runTypechecking file = evalStateT (checkFileM file) ([], Map.empty)

checkFileM :: D.File -> TSState [Decl]
checkFileM file = do
  buildFileScope file
  mapM checkDeclaration file

checkDeclaration :: D.Declaration -> TSState Decl
checkDeclaration d = case d of
 (D.Function name t args body) -> do
   typ <- typeDeclToType "" t
   (argTypes, retType) <- case typ of
     (Function ats rt) ->
       return (ats, rt)
     _                 ->
       err $ "function with non-function type: " ++ show t
   if length argTypes /= length args
     then err "arg length mismatch in declaration"
     else return ()
   beginScope
   addFuncScope args argTypes
   typedBody <- requireReturnType retType body
   endScope
   return $ StmtDecl $ Let name typ $ Lambda typ args typedBody
 (D.Let name t expr) -> do
   typedE <- exprToTyped expr
   typ <- getFromScope t
   _ <- requireSubtype (typeOf typedE) typ
   return $ StmtDecl $ Let name typ typedE
 (D.TypeDef name t) -> do
   typ <- typeDeclToType name t
   return $ IR.TypeDecl name typ

typeDeclToType :: String -> T.TypeDecl -> TSState Type
typeDeclToType tname t = case t of
  T.TypeName name -> case name of
    "()" -> return Type.Nil
    "String" -> return Type.String
    "Float" -> return Type.Float
    "Int" -> return Type.Int
    "Bool" -> return Type.Bool
    _ ->
      err $ "TODO: handle defining type aliases - " ++ name
  T.Function ats rt -> do
    argTypes <- mapM (typeDeclToType "") ats
    retType <- typeDeclToType "" rt
    return $ Type.Function argTypes retType
  T.Struct fields -> do
    types <- mapM (\(name, ty) -> typeDeclToType name ty) fields
    let names = map fst fields
    return $ Type.Struct tname (zip names types)
  T.Enum options -> do
    let (names, opts) = unzip options
    typedOpts <- mapM convertEnumOption opts
    return $ Type.Enum tname (zip names typedOpts)

convertEnumOption :: T.EnumOption -> TSState [(String, Type)]
convertEnumOption fields = do
  types <- mapM (\(name, t) -> typeDeclToType name t) fields
  let names = map fst fields
  return $ zip names types

requireReturnType :: Type -> S.Statement -> TSState Statement
requireReturnType retType stmt = do
  typedStatement <- checkStatement retType stmt
  lastRetType <- getReturnType typedStatement
  _ <- requireSubtype lastRetType retType
  return typedStatement

getReturnType :: Statement -> TSState Type
getReturnType stmt =
  case stmt of
   (Block t _) -> return $ fromMaybe Type.Nil t
   (Expr e)    -> return $ typeOf e
   _           -> err "function body must be a block or expression"

defaultScope :: TSState ()
defaultScope = do
  setInScope "print" (Type.Function [Type.String] Type.Nil)
  setInScope "()" Type.Nil
  setInScope "String" Type.String
  setInScope "Float" Type.Float
  setInScope "Int" Type.Int
  setInScope "Bool" Type.Bool

-- Defines the types of the arguments within the function's scope
addFuncScope :: [String] -> [Type] -> TSState ()
addFuncScope names types = do
  _ <- mapM (\(n,t) -> setInScope n t) (zip names types)
  return ()

buildFileScope :: D.File -> TSState ()
buildFileScope file = do
  beginScope
  defaultScope
  _ <- mapM addDecl file
  return ()

addDecl :: D.Declaration -> TSState ()
addDecl (D.Let n t _)        = do
  typ <- getFromScope t
  setInScope n typ
addDecl (D.Function n t _ _) = do
  typ <- typeDeclToType "" t
  setInScope n typ
addDecl (D.TypeDef n t) = do
  typ <- typeDeclToType n t
  case typ of
    (Enum _ options) -> do
      setInScope n typ
      _ <- mapM (\(name, _) -> addSubtype name n) options
      _ <- mapM (\(name, fields) -> setInScope name (Struct name fields)) options
      return ()
    _                ->
      setInScope n typ

checkStatement :: Type -> S.Statement -> TSState Statement
checkStatement retType stmt = case stmt of
  (S.Return Nothing) -> do
    _ <- requireSubtype Type.Nil retType
    return $ Return Nothing
  (S.Return (Just e)) -> do
    typedE <- exprToTyped e
    _ <- requireSubtype (typeOf typedE) retType
    return $ Return (Just typedE)
  (S.Let name t e) -> do
    typ <- getFromScope t
    setInScope name typ
    typedE <- exprToTyped e
    _ <- requireSubtype (typeOf typedE) typ
    return $ Let name typ typedE
  (S.Assign names e) -> do
    t <- getAssignType names
    typedE <- exprToTyped e
    _ <- requireSubtype (typeOf typedE) t
    return $ Assign names typedE
  (S.Block stmts) -> do
    beginScope
    blk <- checkBlock retType stmts
    endScope
    return blk
  (S.Expr e) -> do
    typedE <- exprToTyped e
    return $ Expr typedE
  (S.If test body mElse) -> do
    typedTest <- exprToTyped test
    _ <- requireSubtype (typeOf typedTest) Type.Bool

    beginScope
    blk <- checkStatement retType (S.Block body)
    endScope

    typedElse <- case mElse of
      Nothing -> return Nothing
      (Just els) -> do
        tEls <- checkStatement retType els
        return $ Just tEls

    return $ If typedTest blk typedElse
  (S.While test body) -> do
    typedTest <- exprToTyped test
    _ <- requireSubtype (typeOf typedTest) Type.Bool

    beginScope
    blk <- checkStatement retType (S.Block body)
    endScope

    return $ While typedTest blk

checkBlock :: Type -> [S.Statement] -> TSState Statement
checkBlock retType stmts = blkStmts stmts []
  where
    blkStmts sts rest = case sts of
      [] ->
        return $ Block Nothing (reverse rest)
      [S.Return rExpr] -> do
        (typ, stmt) <- returnExpr retType rExpr
        return $ Block typ (reverse $ stmt : rest)
      (S.Return _:ss) ->
        err $ "Unreachable statements after a return: " ++ show ss
      (st:ss) -> do
        typedSt <- checkStatement retType st
        blkStmts ss (typedSt : rest)

returnExpr :: Type -> Maybe E.Expression -> TSState (Maybe Type, Statement)
returnExpr t rExpr = case rExpr of
  Nothing -> do
    _ <- requireSubtype Type.Nil t
    return (Nothing, Return Nothing)
  Just e -> do
    typedE <- exprToTyped e
    let typ = typeOf typedE
    _ <- requireSubtype typ t
    return (Just typ, Return $ Just typedE)

getAssignType :: [String] -> TSState Type
getAssignType []           = err "compiler error: assign statement with no names"
getAssignType (var:fields) = do
  t <- getFromScope var
  foldM getFieldType t fields

valToTyped :: E.Value -> TSState Value
valToTyped (E.StrVal s)       = return $ StrVal s
valToTyped (E.BoolVal b)      = return $ BoolVal b
valToTyped (E.IntVal i)       = return $ IntVal i
valToTyped (E.FloatVal f)     = return $ FloatVal f
valToTyped (E.StructVal s fs) = do
  fs' <- mapMSnd exprToTyped fs
  typ <- getFromScope s
  return $ StructVal typ fs'

exprToTyped :: E.Expression -> TSState Expression
exprToTyped e = case e of
 (E.Paren inner) -> do
   typedInner <- exprToTyped inner
   let innerType = typeOf typedInner
   return $ Paren typedInner innerType
 (E.Val value) -> do
   typedVal <- valToTyped value
   return $ Val typedVal
 (E.Unary op e') -> do
   typedInner <- exprToTyped e'
   t <- unaryReturnType op (typeOf typedInner)
   return $ Unary t op typedInner
 (E.Binary op l r) -> do
   typedL <- exprToTyped l
   typedR <- exprToTyped r
   argT <- requireEqual (typeOf typedL) (typeOf typedR)
   t <- binaryReturnType op argT
   return $ Binary t op typedL typedR
 (E.Call fnEx argEs) -> do
   typedFn <- exprToTyped fnEx
   typedArgs <- mapM exprToTyped argEs
   checkFnCall typedFn typedArgs
 (E.Cast t e') -> do
   innerExpr <- exprToTyped e'
   typ <- getFromScope t
   return $ Cast typ innerExpr
 (E.Var name) -> do
   t <- getFromScope name
   return $ Var t name
 (E.Access e' name) -> do
   typedInner <- exprToTyped e'
   t <- getFieldType (typeOf typedInner) name
   return $ Access t typedInner name

binaryReturnType :: E.BinOp -> Type -> TSState Type
binaryReturnType op t
  | op == E.Plus =
    -- special case "+" because it also works on strings
    if t `elem` [Type.Int, Type.Float, Type.String]
    then return t
    else err $ "Can't add values of type: " ++ show t
  | op `elem` numericOps = requireNumeric t
  | op `elem` integerOps = requireSubtype t Type.Int
  | op `elem` booleanOps = requireSubtype t Type.Bool
  | op `elem` compOps    = return Type.Bool
  | op `elem` numCompOps = do
      _ <- requireNumeric t
      return Type.Bool
  | otherwise            = err $ "op missing from list: " ++ show op

numericOps :: [E.BinOp]
numericOps =
  [E.Minus, E.Times, E.Divide, E.Power]

integerOps :: [E.BinOp]
integerOps =
  [E.Mod, E.BitAnd, E.BitOr, E.BitXor, E.LShift, E.RShift, E.RRShift]

booleanOps :: [E.BinOp]
booleanOps =
  [E.BoolAnd, E.BoolOr]

compOps :: [E.BinOp]
compOps =
  [E.Eq, E.NotEq]

numCompOps :: [E.BinOp]
numCompOps =
  [E.Less, E.LessEq, E.Greater, E.GreaterEq]

unaryReturnType :: E.UnaryOp -> Type -> TSState Type
unaryReturnType op t = case op of
  E.BitInvert -> requireSubtype t (Type.Int)
  E.BoolNot   -> requireSubtype t (Type.Bool)

checkFnCall :: Expression -> [Expression] -> TSState Expression
checkFnCall typedFn typedArgs =
  let fnType = typeOf typedFn
      argTypes = map typeOf typedArgs
  in case fnType of
      (Function argTs retT) ->
        if length argTs /= length typedArgs
        then err $ "argument length mismatch"
        else do
          _ <- zipWithM requireSubtype argTypes argTs
          return $ Call retT typedFn typedArgs
      _ ->
        err $ "calling value of type " ++ show fnType ++ " as a function"

-- once interfaces exist, replace this with the Num interface
requireNumeric :: Type -> TSState Type
requireNumeric t =
  if t `elem` [Type.Int, Type.Float]
  then return t
  else err $ "Expected a numeric value: " ++ show t

requireEqual :: Type -> Type -> TSState Type
requireEqual t1 t2 =
  if t1 == t2 then return t1
  else err ("Expected types to be the same: " ++
            show t1 ++ ", " ++ show t2)

requireSubtype :: Type -> Type -> TSState Type
requireSubtype sub super = do
  isSub <- isSubtype super sub
  if isSub
  then return sub
  else err ("can't use a value of type " ++ show sub ++
            " where a value of type " ++ show super ++ " is expected")

isSubtype :: Type -> Type -> TSState Bool
isSubtype super sub
  | sub == super = return True
  | otherwise    = do
    -- not efficient, but that can be fixed later
      let subName = Type.nameOf sub
      supers <- getSuperTypesOf subName
      superList <- mapM getFromScope (Set.toAscList supers)
      matches <- mapM (isSubtype super) superList
      return $ anyTrue matches

anyTrue :: [Bool] -> Bool
anyTrue = foldl (||) False

getFieldType :: Type -> String -> TSState Type
getFieldType typ field = do
  fieldTypes <- getStructFields typ
  let errMsg = "field " ++ field ++ " not on type " ++ show typ
  lift $ note errMsg (lookup field fieldTypes)

getStructFields :: Type -> TSState [(String, Type)]
getStructFields (Struct _ fields) =
  return fields
getStructFields t =
  err $ "Can't access field on a value of type " ++ show t

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd f = map (\(c, a) -> (c, f a))

mapMSnd :: (Monad m) => (a -> m b) -> [(c, a)] -> m [(c, b)]
mapMSnd f = mapM f'
  where f' (c, a) = do
          b <- f a
          return (c, b)
