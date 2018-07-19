module FirstPass where

import Data.Map (Map)
import qualified Data.Map as Map

import AST.Declaration (Declaration, File)
import AST.Type (TypeDecl)
import Errors
  ( Error(..)
  , Result )


data Module =
  Module
  { bindings :: Map String Declaration
  , types :: Map String TypeDecl
  }

-- firstPass is the first thing run after parsing.
-- It prepares data for type inference.
--
-- Among other things, this:
-- * Checks for duplicate bindings
-- * Splits types from other bindings
-- * Creates extra type bindings for the "functions" that construct
--   structures.
-- * Lowers the syntax somewhat (e.g. StructVal -> Call)
firstPass :: File -> Result Module
firstPass = undefined

-- TODO: add the ability to combine multiple files into a module,
--   but check imports on a per-file basis
