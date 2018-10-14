module FirstPass where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import AST.Declaration
  ( Declaration(..)
  , File
  , getDeclaredName )
import qualified AST.Statement as S
import AST.Type ( TypeDecl )
import qualified AST.Type as T
import Errors
  ( Error(..)
  , Result )


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
-- * (TODO) Creates extra type bindings for the "functions" that construct
--   structures.
-- * (TODO) Check that all types referred to actually exist
-- * (TODO) Check that variable declarations are unique in a given block
-- * (TODO) Check for variables that are defined and never referenced
-- * (TODO) Check that match statements have at least one case
-- * (TODO) Check that variables in match expressions are not repeated
-- * (TODO) Check that match cases do not completely overlap
-- * (TODO) Check that match cases are complete? (at least, for match expressions)
firstPass :: File -> Result Module
firstPass file = do
  typesFound <- gatherTypeDecls file
  enumTs <- gatherEnumTypes file
  binds <- gatherBindings file
  mapM_ checkReturns binds
  return $ Module { bindings=binds, types=typesFound, enumTypes=enumTs }


gatherEnumTypes :: File -> Result (Map String String)
gatherEnumTypes file = do
  let enumDecls = [(name, options) | TypeDef _ name (T.Enum options) <- file]
  let optionNames = [ (optName, name)
                    | (name, options) <- enumDecls
                    , (optName, _) <- options ]
  return $ Map.fromList optionNames

-- select and deduplicate type declarations
-- TODO: Support type alises
gatherTypeDecls :: File -> Result (Map String TypeDecl)
gatherTypeDecls file = do
  let typeDecls = [(name, t) | TypeDef _ name t <- file]
  unnestedTypeDecls <- unnestAll typeDecls
  let addDecl ds (name, t) = case Map.lookup name ds of
        Nothing ->
          return $ Map.insert name t ds
        Just _ ->
          duplicateName name
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
  T.TypeName _ ->
    return (tdecl, [])
  T.Function args ret -> do
    mapM_ noStructures args
    noStructures ret
    return (tdecl, [])
  T.Struct fields -> do
    requireUnique $ map fst fields
    (unnested, decls) <- unnestFields name fields
    return (T.Struct unnested, decls)
  T.Enum variants -> do
    requireUnique $ map fst variants
    structs <- mapM (uncurry variantToStruct) variants
    return (tdecl, structs)


variantToStruct :: String -> T.EnumOption -> Result (String, TypeDecl)
variantToStruct name fields = do
  let (names, fieldTypes) = unzip fields
  requireUnique names
  mapM_ noStructures fieldTypes
  return (name, T.Struct fields)


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
  T.Function args ret -> do
    mapM_ noStructures args
    noStructures ret
  T.Struct{}          -> Left InvalidAnonStructure
  T.Enum{}            -> Left InvalidAnonStructure


requireUnique :: [String] -> Result ()
requireUnique items = requireUnique' items Set.empty

requireUnique' :: [String] -> Set String -> Result ()
requireUnique' []        _    = return ()
requireUnique' (n:names) seen =
  if Set.member n seen
  then duplicateName n
  else requireUnique' names (Set.insert n seen)

-- select and deduplicate function and let bindings
gatherBindings :: File -> Result (Map String Declaration)
gatherBindings file =
  let binds = filter (not . isTypeDecl) file
      addBinding bs decl =
        let name = getDeclaredName decl
        in case Map.lookup name bs of
            Nothing -> return $ Map.insert name decl bs
            Just _  -> duplicateName name
  in foldM addBinding Map.empty binds

checkReturns :: Declaration -> Result ()
checkReturns TypeDef{} =
  return ()
checkReturns Let{} =
  return ()
checkReturns (Function _ name _ _ stmt) = do
  _ <- checkStmtsReturn name Never [stmt]
  return ()

checkStmtsReturn :: String -> DoesReturn -> [S.Statement] -> Result DoesReturn
checkStmtsReturn fname prevReturns stmts =
  case prevReturns of
   Always -> case stmts of
     []    -> return Always
     (_:_) -> Left $ Unreachable fname
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
