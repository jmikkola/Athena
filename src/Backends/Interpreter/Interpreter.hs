module Backends.Interpreter.Interpreter where

import System.Exit ( ExitCode(..) )
import Data.Map (Map)

import IR
import Type (Type, TypeRef)

run :: Map TypeRef Type -> [IR.Decl] -> IO ExitCode
run types file = do
  putStrLn "todo"
  return ExitSuccess
