module FirstPass where

import Control.Monad (foldM)
import Data.Map (Map)
import qualified Data.Map as Map

import AST.Declaration
  ( Declaration(..)
  , File
  , getDeclaredName )
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
-- * (TODO) Lowers the syntax somewhat (e.g. StructVal -> Call)
-- * (TODO) Check that all types referred to actually exist
-- * (TODO) Check that variable declarations are unique in a given block
firstPass :: File a -> Result (Module a)
firstPass file = do
  typesFound <- gatherTypeDecls file
  binds <- gatherBindings file
  return $ Module { bindings=binds, types=typesFound }

-- select and deduplicate type declarations
gatherTypeDecls :: File a -> Result (Map String TypeDecl)
gatherTypeDecls file =
  let typeDecls = [(name, t) | TypeDef _ name t <- file]
      addDecl ds (name, t) = case Map.lookup name ds of
        Nothing -> return $ Map.insert name t ds
        Just _  -> duplicateName name
  in foldM addDecl Map.empty typeDecls

-- selet and deduplicate function and let bindings
gatherBindings :: File a -> Result (Map String (Declaration a))
gatherBindings file =
  let binds = filter (not . isTypeDecl) file
      addBinding bs decl =
        let name = getDeclaredName decl
        in case Map.lookup name bs of
            Nothing -> return $ Map.insert name decl bs
            Just _  -> duplicateName name
  in foldM addBinding Map.empty binds

isTypeDecl :: Declaration a -> Bool
isTypeDecl (TypeDef _ _ _) = True
isTypeDecl _               = False

-- TODO: add the ability to combine multiple files into a module,
--   but check imports on a per-file basis

duplicateName :: String -> Result a
duplicateName name = Left $ DuplicateBinding name
