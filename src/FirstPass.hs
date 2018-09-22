module FirstPass where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map

import AST.Declaration
  ( Declaration(..)
  , File
  , getDeclaredName )
import qualified AST.Statement as S
import AST.Type (TypeDecl)
import Errors
  ( Error(..)
  , Result )


data Module a =
  Module
  { bindings :: Map String (Declaration a)
  , types :: Map String TypeDecl
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
firstPass :: File a -> Result (Module a)
firstPass file = do
  typesFound <- gatherTypeDecls file
  binds <- gatherBindings file
  mapM_ checkReturns binds
  return $ Module { bindings=binds, types=typesFound }

-- select and deduplicate type declarations
gatherTypeDecls :: File a -> Result (Map String TypeDecl)
gatherTypeDecls file =
  let typeDecls = [(name, t) | TypeDef _ name t <- file]
      addDecl ds (name, t) = case Map.lookup name ds of
        Nothing -> return $ Map.insert name t ds
        Just _  -> duplicateName name
  in foldM addDecl Map.empty typeDecls

-- select and deduplicate function and let bindings
gatherBindings :: File a -> Result (Map String (Declaration a))
gatherBindings file =
  let binds = filter (not . isTypeDecl) file
      addBinding bs decl =
        let name = getDeclaredName decl
        in case Map.lookup name bs of
            Nothing -> return $ Map.insert name decl bs
            Just _  -> duplicateName name
  in foldM addBinding Map.empty binds

checkReturns :: Declaration a -> Result ()
checkReturns TypeDef{} =
  return ()
checkReturns Let{} =
  return ()
checkReturns (Function _ name _ stmt) = do
  _ <- checkStmtsReturn name Never [stmt]
  return ()

checkStmtsReturn :: String -> DoesReturn -> [S.Statement a] -> Result DoesReturn
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

isTypeDecl :: Declaration a -> Bool
isTypeDecl TypeDef{} = True
isTypeDecl _         = False

-- TODO: add the ability to combine multiple files into a module,
--   but check imports on a per-file basis

duplicateName :: String -> Result a
duplicateName name = Left $ DuplicateBinding name
