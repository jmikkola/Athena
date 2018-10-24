module FirstPass where

import Control.Monad (foldM)
import Data.Foldable (forM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

import AST.Annotation (Annotated, getLocation)
import AST.Declaration
  ( Declaration(..)
  , File
  , getDeclaredName )
import qualified AST.Expression as E
import qualified AST.Statement as S
import AST.Type ( TypeDecl )
import qualified AST.Type as T
import Errors
  ( Error(..)
  , Result )
import Util.Functions

data Module =
  Module
  { bindings :: Map String Declaration
  , types :: Map String TypeDecl
  , enumTypes :: Map String String
  }
  deriving (Show)

-- firstPass is the first thing run after parsing.
-- It prepares data for type inference.
--
-- Among other things, this:
-- * Checks for duplicate bindings
-- * Splits types from other bindings
-- * (TODO) Check that all types referred to actually exist
-- * (TODO) Check for variables that are defined and never referenced
-- * (TODO) Check that match statements have at least one case
-- * (TODO) Check that match cases do not completely overlap
firstPass :: File -> Result Module
firstPass file = do
  uniqueDecls <- ensureDeclsAreUnique file
  typesFound <- gatherTypeDecls uniqueDecls
  enumTs <- gatherEnumTypes file
  binds <- gatherBindings uniqueDecls
  mapM_ checkReturns binds
  mapM_ checkDupVars binds
  return Module { bindings=binds, types=typesFound, enumTypes=enumTs }


type DeclMap = Map String Declaration

ensureDeclsAreUnique :: File -> Result DeclMap
ensureDeclsAreUnique [] = return Map.empty
ensureDeclsAreUnique (d:ds) = do
  rest <- ensureDeclsAreUnique ds
  let name = getDeclaredName d
  case Map.lookup name rest of
    Nothing ->
      return $ Map.insert name d rest
    Just duplicate ->
      withLocations [d, duplicate] $ duplicateName name


gatherEnumTypes :: File -> Result (Map String String)
gatherEnumTypes file = do
  let enumDecls = [(name, options) | TypeDef _ name (T.Enum _ options) <- file]
  let optionNames = [ (optName, name)
                    | (name, options) <- enumDecls
                    , (optName, _) <- options ]
  return $ Map.fromList optionNames

-- select and deduplicate type declarations
-- TODO: Support type alises
gatherTypeDecls :: DeclMap -> Result (Map String TypeDecl)
gatherTypeDecls decls = do
  let list = Map.toList decls
  let typeDecls = [(name, t) | (_, TypeDef _ name t) <- list]
  unnestedTypeDecls <- unnestAll typeDecls
  -- Trust that ensureDeclsAreUnique has already made the declarations unique
  -- and that the unnest scheme works at generating unique names
  let addDecl ds (name, t) = return $ Map.insert name t ds
  foldM addDecl Map.empty unnestedTypeDecls


type TypeDecls = [(String, TypeDecl)]

unnestAll :: TypeDecls -> Result TypeDecls
unnestAll [] = return []
unnestAll ((name,t):tds) = do
  (decl, unnested) <- unnestStructures name t
  rest <- unnestAll tds
  return $ (name, decl) : unnested ++ rest

unnestStructures :: String -> TypeDecl -> Result (TypeDecl, TypeDecls)
unnestStructures name tdecl = case tdecl of
  -- Aliases will have to happen as a second pass
  T.TypeName _ _ ->
    return (tdecl, [])
  T.Function _ args ret -> do
    mapM_ noStructures args
    noStructures ret
    return (tdecl, [])
  T.Struct a fields -> do
    requireUnique $ map fst fields
    (unnested, decls) <- unnestFields name fields
    return (T.Struct a unnested, decls)
  T.Enum _ variants -> do
    requireUnique $ map fst variants
    structs <- mapM (uncurry variantToStruct) variants
    return (tdecl, structs)


variantToStruct :: String -> T.EnumOption -> Result (String, TypeDecl)
variantToStruct name fields = do
  let (names, fieldTypes) = unzip fields
  requireUnique names
  mapM_ noStructures fieldTypes
  return (name, T.Struct [] fields)


-- fields -> Result (fields, de-anonymized)
unnestFields :: String -> TypeDecls -> Result (TypeDecls, TypeDecls)
unnestFields _    []         = return ([], [])
unnestFields name ((n,t):ts) = do
  (unnested, decls) <- unnestStructures (name ++ "." ++ n) t
  (rest, decls') <- unnestFields name ts
  return ((n, unnested) : rest, decls ++ decls')


noStructures :: TypeDecl -> Result ()
noStructures tdecl = case tdecl of
  T.TypeName{}        -> return ()
  T.Function _ args ret -> do
    mapM_ noStructures args
    noStructures ret
  T.Struct{}          -> withLocations [tdecl] $ Left InvalidAnonStructure
  T.Enum{}            -> withLocations [tdecl] $ Left InvalidAnonStructure


requireUnique :: [String] -> Result ()
requireUnique items = requireUnique' items Set.empty

requireUnique' :: [String] -> Set String -> Result ()
requireUnique' []        _    = return ()
requireUnique' (n:names) seen =
  if Set.member n seen
  then duplicateName n
  else requireUnique' names (Set.insert n seen)

-- select and deduplicate function and let bindings
gatherBindings :: DeclMap -> Result (Map String Declaration)
gatherBindings decls =
  let binds = Map.filter (not . isTypeDecl) decls
      addBinding bs decl =
        return $ Map.insert (getDeclaredName decl) decl bs
  in foldM addBinding Map.empty binds

checkReturns :: Declaration -> Result ()
checkReturns TypeDef{} =
  return ()
checkReturns Let{} =
  return ()
checkReturns (Function _ name _ _ stmt) = do
  _ <- checkStmtsReturn name Never [stmt]
  return ()

checkDupVars :: Declaration -> Result ()
checkDupVars decl = case decl of
  TypeDef{}          -> return ()
  Let _ _ _ e        -> checkDupVarsExpr e
  Function _ _ _ _ s -> checkDupVarsStmt s


checkDupVarsStmt :: S.Statement -> Result ()
checkDupVarsStmt stmt = case stmt of
  S.Return _ me      -> forM_ me checkDupVarsExpr
  S.Let _ _ _ e      -> checkDupVarsExpr e
  S.Assign _ _ e     -> checkDupVarsExpr e
  S.Block _ stmts    -> checkDupVarsBlock stmts Set.empty
  S.Expr _ e         -> checkDupVarsExpr e
  S.If _ e stmts els -> do
    checkDupVarsExpr e
    checkDupVarsBlock stmts Set.empty
    forM_ els checkDupVarsStmt
  S.While _ e stmts  -> do
    checkDupVarsExpr e
    checkDupVarsBlock stmts Set.empty
  S.Match _ e cases  -> do
    checkDupVarsExpr e
    mapM_ checkDupVarsCase cases


checkDupVarsBlock :: [S.Statement] -> Set String -> Result ()
checkDupVarsBlock [] _ = return ()
checkDupVarsBlock (s:ss) declared = do
  checkDupVarsStmt s
  case s of
    S.Let _ name _ _ ->
      if Set.member name declared
      then withLocations [s] $ Left $ DuplicateBinding name
      else checkDupVarsBlock ss (Set.insert name declared)
    _ ->
      return ()

checkDupVarsCase :: S.MatchCase -> Result ()
checkDupVarsCase (S.MatchCase me st) = do
  checkDupVarsME me
  checkDupVarsStmt st

checkDupVarsME :: S.MatchExpression -> Result ()
checkDupVarsME me =
  checkDuplicateBindings (gatherMEBindings me) Set.empty

gatherMEBindings :: S.MatchExpression -> [(String, S.MatchExpression)]
gatherMEBindings me = case me of
  S.MatchAnything _        -> []
  S.MatchVariable _ s      -> [(s, me)]
  S.MatchStructure _ _ mes ->
    concatMap gatherMEBindings mes

checkDuplicateBindings :: (Annotated ast) => [(String, ast)] -> Set String -> Result ()
checkDuplicateBindings []                  _        =
  return ()
checkDuplicateBindings ((name, node):rest) declared =
  if Set.member name declared
  then withLocations [node] $ Left $ DuplicateBinding name
  else checkDuplicateBindings rest (Set.insert name declared)


-- Update this once closures exist
checkDupVarsExpr :: E.Expression -> Result ()
checkDupVarsExpr _ = return ()


checkStmtsReturn :: String -> DoesReturn -> [S.Statement] -> Result DoesReturn
checkStmtsReturn fname prevReturns stmts =
  case prevReturns of
   Always -> case stmts of
     []    -> return Always
     (s:_) -> withLocations [s] $ Left $ Unreachable fname
   _ -> case stmts of
     []     -> return prevReturns
     (s:ss) -> case s of
       S.Return _ _ ->
         checkStmtsReturn fname Always ss
       S.Block _ blk -> do
         returns <- checkStmtsReturn fname prevReturns blk
         checkStmtsReturn fname returns ss
       S.If _ _ thenCase Nothing -> do
         returns <- checkStmtsReturn fname prevReturns thenCase
         let actuallyReturns = if returns == Always then Sometimes else returns
         checkStmtsReturn fname actuallyReturns ss
       S.If _ _ thenCase (Just elseCase) -> do
         thenReturns <- checkStmtsReturn fname prevReturns thenCase
         elseReturns <- checkStmtsReturn fname prevReturns [elseCase]
         let actuallyReturns = case (thenReturns, elseReturns) of
               (Always, Always) -> Always
               (Never,  Never)  -> Never
               (_,      _)      -> Sometimes
         checkStmtsReturn fname actuallyReturns ss
       S.While _ _ whileBody -> do
         whileReturns <- checkStmtsReturn fname prevReturns whileBody
         let actuallyReturns = if whileReturns == Always then Sometimes else whileReturns
         checkStmtsReturn fname actuallyReturns ss
       _ ->
         checkStmtsReturn fname prevReturns ss

data DoesReturn
  = Never
  | Sometimes
  | Always
  deriving (Eq, Show)

isTypeDecl :: Declaration -> Bool
isTypeDecl TypeDef{} = True
isTypeDecl _         = False

-- TODO: add the ability to combine multiple files into a module,
--   but check imports on a per-file basis

duplicateName :: String -> Result a
duplicateName name = Left $ DuplicateBinding name

withLocations :: (Annotated a) => [a] -> Result b -> Result b
withLocations code =
  mapLeft (WithLocations (mapMaybe getLocation code))
