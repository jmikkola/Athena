module Backends.Go.Emit where

import Control.Monad.State

import qualified Backends.Go.Syntax as Syntax

data Output = Output { indent :: Int
                     , pieces :: [String]
                     }
type Result = Either String
type EmitState = StateT Output Result

write :: String -> EmitState ()
write s = modify (\o -> o { pieces = (s : pieces o) })

getIndent :: EmitState Int
getIndent = do
  o <- get
  return $ indent o

alterIndent :: Int -> EmitState ()
alterIndent delta = modify (\o -> o { indent = delta + indent o })

increaseIndent :: EmitState ()
increaseIndent = alterIndent 1

decreateIndent :: EmitState ()
decreateIndent = alterIndent (-1)

writeIndent :: EmitState ()
writeIndent = do
  n <- getIndent
  write $ indentString n

indentString :: Int -> String
indentString n = replicate n '\t'
