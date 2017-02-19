module TypeCheck where

import Control.Applicative ( (<|>) )
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import System.IO.Unsafe

import IR
import qualified AST.Declaration as D
import qualified AST.Expression as E
import qualified AST.Statement as S
import qualified AST.Type as T
import Type
import Matching

type TypeName = String

type Result = Either String
type Scope = Map String TypeRef
type TypeScope = [Scope]
type Subtypes = Map TypeRef (Set TypeRef)
type EnumVariants = Map TypeRef (Set TypeRef)
type TypeMap = Map TypeRef Type
data TypeCheckState
  = TypeCheckState
    { varScope :: [Scope]
    , types :: TypeMap
    , subtypes :: Subtypes
    , enumVariants :: EnumVariants
    }
type TSState = StateT TypeCheckState Result

-- types

nilT :: TypeRef
nilT = "()"

-- Monad functions --

getVarScope :: TSState TypeScope
getVarScope = liftM varScope $ get

getTypes :: TSState (TypeMap)
getTypes = liftM types $ get

getEnumVariants :: TSState (EnumVariants)
getEnumVariants = liftM enumVariants $ get

getSubtypes :: TSState Subtypes
getSubtypes = liftM subtypes $ get

putVarScope :: TypeScope -> TSState ()
putVarScope vars = modify (\s -> s { varScope = vars })

putTypes :: TypeMap -> TSState ()
putTypes typs = modify (\s -> s { types = typs })

putSubtypes :: Subtypes -> TSState ()
putSubtypes subs = modify (\s -> s { subtypes = subs })

putEnumVariants :: EnumVariants -> TSState ()
putEnumVariants variants = modify (\s -> s { enumVariants = variants })

updateVarScope :: (TypeScope -> TypeScope) -> TSState ()
updateVarScope f = do
  ts <- getVarScope
  putVarScope (f ts)

updateSubtypes :: (Subtypes -> Subtypes) -> TSState ()
updateSubtypes f = do
  subs <- getSubtypes
  putSubtypes (f subs)

updateEnumVariants :: (EnumVariants -> EnumVariants) -> TSState ()
updateEnumVariants f = do
  variants <- getEnumVariants
  putEnumVariants (f variants)

-- Scaffolding functions --

err :: String -> TSState a
err = lift . Left

note :: a -> Maybe b -> Either a b
note msg = maybe (Left msg) Right

beginScope :: TSState ()
beginScope = updateVarScope (Map.empty :)

endScope :: TSState ()
endScope = do
  ts <- getVarScope
  case ts of
   []     -> err "compiler bug: ending no scopes"
   (_:ss) -> putVarScope ss

scopeLookup :: String -> TypeScope -> Maybe TypeRef
scopeLookup name (m:ms) = Map.lookup name m <|> scopeLookup name ms
scopeLookup _    []     = Nothing

getFromScope :: String -> TSState TypeRef
getFromScope name = do
  ts <- getVarScope
  lift $ note ("Not defined: " ++ name) (scopeLookup name ts)

defineVar :: String -> TypeRef -> TypeScope -> Result TypeScope
defineVar name typ (m:ms) =
  case Map.lookup name m of
   Nothing  -> return (Map.insert name typ m : ms)
   (Just _) -> Left ("Duplicate definition for: " ++ name)
defineVar _    _   []     =
  Left "compiler bug: empty scope stack??"

setVarInScope :: String -> TypeRef -> TSState ()
setVarInScope name typ = do
  ts <- getVarScope
  newScopes <- lift $ defineVar name typ ts
  putVarScope newScopes

defineType :: TypeRef -> Type -> TypeMap -> Result TypeMap
defineType name typ types =
  case Map.lookup name types of
   Nothing  -> return (Map.insert name typ types)
   (Just t) ->
     -- Allow identical anonymouse types to share a name
     if t == typ && name == genName typ
     then return types
     else Left ("Duplicate definition for: " ++ name)

addType :: TypeRef -> Type -> TSState ()
addType name typ = do
  ts <- getTypes
  newTypes <- lift $ defineType name typ ts
  putTypes newTypes

getType :: TypeRef -> TSState Type
getType name = do
  ts <- getTypes
  case Map.lookup name ts of
   (Just t) -> return t
   Nothing  -> err $ "undefined type: " ++ name

addSubtype :: TypeRef -> TypeRef -> TSState ()
addSubtype super sub = updateSubtypes addSub
  where addSub subtypes =
          let newSupers = case Map.lookup sub subtypes of
                Nothing     -> Set.singleton super
                (Just sups) -> Set.insert super sups
          in Map.insert sub newSupers subtypes

addEnumVariant :: TypeRef -> TypeRef -> TSState ()
addEnumVariant enumName optionName = updateEnumVariants addVariant
  where addVariant variants =
          let existing = fromMaybe Set.empty $ Map.lookup enumName variants
              new = Set.insert optionName existing
          in Map.insert enumName new variants

getSuperTypesOf :: TypeRef -> TSState (Set TypeRef)
getSuperTypesOf sub = do
  subs <- getSubtypes
  -- TODO: traverse more than just one level
  case Map.lookup sub subs of
   Nothing -> return Set.empty
   Just st -> return st

-- Typing functions --

runTypechecking :: D.File -> Result (TypeMap, [Decl])
runTypechecking file = evalStateT (checkFileM file) startingState

startingState :: TypeCheckState
startingState =
  let printType = Type.Function ["String"] nilT
      printTypeName = Type.genName printType
  in TypeCheckState
     { varScope = [Map.singleton "print" printTypeName]
     , types = Map.fromList
               [ (printTypeName, printType)
               , (nilT, Type.Nil)
               , ("String", Type.String)
               , ("Float", Type.Float)
               , ("Int", Type.Int)
               , ("Bool", Type.Bool)
               ]
     , subtypes = Map.empty
     }

checkFileM :: D.File -> TSState (TypeMap, [Decl])
checkFileM file = do
  gatherNamedTypes file
  gatherVarBindings file
  declarations <- mapM checkDeclaration file
  validateTypes
  types <- getTypes
  return (types, declarations)

gatherNamedTypes :: D.File -> TSState ()
gatherNamedTypes file = do
  _ <- mapM addDeclaredTypes file
  return ()

addDeclaredTypes :: D.Declaration -> TSState ()
addDeclaredTypes d = case d of
  (D.TypeDef name t) ->
    declareType name t
  _                  ->
    return () -- no other ways for types to be declared

declareType :: TypeRef -> T.TypeDecl -> TSState ()
declareType name t = case t of
  -- This isn't allowed at this level; another pass needs to be added
  -- to resolve aliasing
  T.TypeName alias ->
    err $ "can't alias type " ++ name ++ " to " ++ alias
  T.Function argTs rt -> do
    argTypes <- mapM ensureAdded argTs
    retType <- ensureAdded rt
    addType name (Type.Function argTypes retType)
  T.Struct fields -> do
    typedFields <- mapMSnd ensureAdded fields
    addType name (Type.Struct typedFields)
  T.Enum options -> do
    typedOptions <- mapMSnd ensureOptionAdded options
    optionNames <- mapM addOptionType typedOptions
    _ <- mapM (addSubtype name) optionNames
    _ <- mapM (addEnumVariant name) optionNames
    addType name (Type.Enum typedOptions)

addOptionType :: (String, [(String, TypeRef)]) -> TSState TypeRef
addOptionType (name, fields) = do
  addType name $ Type.Struct fields
  return name

ensureAdded :: T.TypeDecl -> TSState TypeRef
ensureAdded t = case t of
  T.TypeName name ->
    return name
  T.Function argTs rt -> do
    argTypes <- mapM ensureAdded argTs
    retType <- ensureAdded rt
    addAnonType $ Type.Function argTypes retType
  T.Struct fields -> do
    typedFields <- mapMSnd ensureAdded fields
    addAnonType $ Type.Struct typedFields
  T.Enum options -> do
    typedOptions <- mapMSnd ensureOptionAdded options
    addAnonType $ Type.Enum typedOptions

ensureOptionAdded :: T.EnumOption -> TSState [(String, TypeRef)]
ensureOptionAdded = mapMSnd ensureAdded

addAnonType :: Type -> TSState TypeRef
addAnonType typ = do
  let name = Type.genName typ
  addType name typ
  return name

-- Validate types ensures that no type contains a reference to
-- types that don't exist.
validateTypes :: TSState ()
validateTypes = do
  typs <- getTypes
  _ <- mapM validateType $ Map.elems typs
  return ()

validateType :: Type -> TSState ()
validateType typ = case typ of
  Function atyps rtyp -> do
    _ <- mapM getType (rtyp : atyps)
    return ()
  Struct fields -> do
    _ <- mapMSnd getType fields
    return ()
  Enum options -> do
    _ <- mapMSnd (\option -> mapMSnd getType option) options
    return ()
  _ ->
    return () -- no way for other types to be invalid

-- Adds module-level bindings and their types
gatherVarBindings :: D.File -> TSState ()
gatherVarBindings file = do
  _ <- mapM addDecl file
  return ()

-- Adds names and their types to the module-level scope
addDecl :: D.Declaration -> TSState ()
addDecl (D.Let n t _)        = do
  _ <- getType t -- ensure type exists
  setVarInScope n t
addDecl (D.Function n t _ _) = do
  tname <- ensureAdded t
  setVarInScope n tname
addDecl (D.TypeDef n t) =
  return () -- already dealt with in the gatherNamedTypes phase

-- Typechecks the contents of declarations
checkDeclaration :: D.Declaration -> TSState Decl
checkDeclaration d = case d of
 (D.Function name t args body) -> do
   tname <- ensureAdded t
   typ <- getType tname
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
   return $ StmtDecl $ Let name tname $ Lambda tname args typedBody
 (D.Let name tname expr) -> do
   typedE <- exprToTyped expr
   _ <- getType tname -- ensure exists
   _ <- requireSubtype (typeOf typedE) tname
   return $ StmtDecl $ Let name tname typedE
 (D.TypeDef name t) -> do
   typ <- getType name -- added in the gatherNamedTypes phase
   return $ IR.TypeDecl name typ

requireReturnType :: TypeRef -> S.Statement -> TSState Statement
requireReturnType retType stmt = do
  typedStatement <- checkStatement retType stmt
  lastRetType <- getReturnType typedStatement
  _ <- requireSubtype lastRetType retType
  return typedStatement

getReturnType :: Statement -> TSState TypeRef
getReturnType stmt =
  case stmt of
   (Block t _) -> return $ fromMaybe nilT t
   (Expr e)    -> return $ typeOf e
   _           -> err "function body must be a block or expression"

-- Defines the types of the arguments within the function's scope
addFuncScope :: [String] -> [TypeRef] -> TSState ()
addFuncScope names types = do
  _ <- mapM (\(n,t) -> setVarInScope n t) (zip names types)
  return ()

checkStatement :: TypeRef -> S.Statement -> TSState Statement
checkStatement retType stmt = case stmt of
  (S.Return Nothing) -> do
    _ <- requireSubtype nilT retType
    return $ Return Nothing
  (S.Return (Just e)) -> do
    typedE <- exprToTyped e
    _ <- requireSubtype (typeOf typedE) retType
    return $ Return (Just typedE)
  (S.Let name tname e) -> do
    _ <- getType tname
    setVarInScope name tname
    typedE <- exprToTyped e
    _ <- requireSubtype (typeOf typedE) tname
    return $ Let name tname typedE
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
    _ <- requireSubtype (typeOf typedTest) "Bool"

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
    _ <- requireSubtype (typeOf typedTest) "Bool"

    beginScope
    blk <- checkStatement retType (S.Block body)
    endScope

    return $ While typedTest blk
  (S.Match expr cases) -> do
    typedExpr <- exprToTyped expr
    -- TODO: Make sure cases don't shadow each other
    -- TODO: Make sure all cases are covered
    -- TODO: disallow duplicate match var names in a given case
    typedCases <- mapM (caseToTyped retType (typeOf typedExpr)) cases
    return $ Match typedExpr typedCases

caseToTyped :: TypeRef -> TypeRef -> S.MatchCase -> TSState MatchCase
caseToTyped retType matchedType (S.MatchCase match body) = do
  beginScope
  typedMatch <- checkMatch matchedType match
  typedBody <- checkStatement retType body
  endScope
  return $ MatchCase typedMatch typedBody

checkMatch :: TypeRef -> S.MatchExpression -> TSState MatchExpression
checkMatch matchedType matchExpr = case matchExpr of
  S.MatchAnything -> return MatchAnything
  S.MatchVariable name -> do
    setVarInScope name matchedType
    return $ MatchVariable name
  S.MatchStructure typeName fields -> do
    typ <- getType typeName
    -- TODO: this isn't quite right (e.g. this shouldn't allow matching Pair<1,1> if the
    -- matched expression is Pair<a,a>). Fixing this probably requires making enums more of a
    -- first-class concept.
    _ <- requireSubtype typeName matchedType
    fieldTypes <- getStructFields typ
    typedFields <- mapM (\((_,t),f) -> checkMatch t f) (zip fieldTypes fields)
    return $ MatchStructure typeName typedFields

checkBlock :: TypeRef -> [S.Statement] -> TSState Statement
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

returnExpr :: TypeRef -> Maybe E.Expression -> TSState (Maybe TypeRef, Statement)
returnExpr t rExpr = case rExpr of
  Nothing -> do
    _ <- requireSubtype nilT t
    return (Nothing, Return Nothing)
  Just e -> do
    typedE <- exprToTyped e
    let typ = typeOf typedE
    _ <- requireSubtype typ t
    return (Just typ, Return $ Just typedE)

getAssignType :: [String] -> TSState TypeRef
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
  _ <- getType s
  return $ StructVal s fs'

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
   _ <- getType t
   return $ Cast t innerExpr
 (E.Var name) -> do
   t <- getFromScope name
   return $ Var t name
 (E.Access e' name) -> do
   typedInner <- exprToTyped e'
   tref <- getFieldType (typeOf typedInner) name
   return $ Access tref typedInner name

binaryReturnType :: E.BinOp -> TypeRef -> TSState TypeRef
binaryReturnType op t
  | op == E.Plus =
    -- special case "+" because it also works on strings
    if t `elem` ["Int", "Float", "String"]
    then return t
    else err $ "Can't add values of type: " ++ show t
  | op `elem` numericOps = requireNumeric t
  | op `elem` integerOps = requireSubtype t "Int"
  | op `elem` booleanOps = requireSubtype t "Bool"
  | op `elem` compOps    = return "Bool"
  | op `elem` numCompOps = do
      _ <- requireNumeric t
      return "Bool"
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

unaryReturnType :: E.UnaryOp -> TypeRef -> TSState TypeRef
unaryReturnType op t = case op of
  E.BitInvert -> requireSubtype t "Int"
  E.BoolNot   -> requireSubtype t "Bool"

checkFnCall :: Expression -> [Expression] -> TSState Expression
checkFnCall typedFn typedArgs =
  let fnTypeRef = typeOf typedFn
      argTypes = map typeOf typedArgs
  in do
    fnType <- getType fnTypeRef
    case fnType of
      (Function argTs retT) ->
        if length argTs /= length typedArgs
        then err $ "argument length mismatch"
        else do
          _ <- zipWithM requireSubtype argTypes argTs
          return $ Call retT typedFn typedArgs
      _ ->
        err $ "calling value of type " ++ show fnType ++ " as a function"

-- once interfaces exist, replace this with the Num interface
requireNumeric :: TypeRef -> TSState TypeRef
requireNumeric t =
  if t `elem` ["Int", "Float"]
  then return t
  else err $ "Expected a numeric value: " ++ show t

requireEqual :: TypeRef -> TypeRef -> TSState TypeRef
requireEqual t1 t2 =
  if t1 == t2 then return t1
  else err ("Expected types to be the same: " ++
            t1 ++ ", " ++ t2)

requireSubtype :: TypeRef -> TypeRef -> TSState TypeRef
requireSubtype sub super = do
  isSub <- isSubtype super sub
  if isSub
  then return sub
  else err ("can't use a value of type " ++ sub ++
            " where a value of type " ++ super ++ " is expected")

isSubtype :: TypeRef -> TypeRef -> TSState Bool
isSubtype super sub
  | sub == super = return True
  | otherwise    = do
    -- not efficient, but that can be fixed later
      supers <- getSuperTypesOf sub
      let superList = Set.toAscList supers
      matches <- mapM (isSubtype super) superList
      return $ anyTrue matches

anyTrue :: [Bool] -> Bool
anyTrue = foldl (||) False

getFieldType :: TypeRef -> String -> TSState TypeRef
getFieldType tname field = do
  typ <- getType tname
  fieldTypes <- getStructFields typ
  let errMsg = "field " ++ field ++ " not on type " ++ show typ
  fieldTname <- lift $ note errMsg (lookup field fieldTypes)
  return fieldTname

getStructFields :: Type -> TSState [(String, TypeRef)]
getStructFields (Struct fields) =
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
