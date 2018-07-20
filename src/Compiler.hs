module Compiler where

import Errors ( Error(..), Result )
import FirstPass ( Module, firstPass )
import Inference ( inferModule )
import Parser ( parseFile )

compile :: String -> Result Module
compile text = do
  file <- mapLeft ParseError $ parseFile text
  m <- firstPass file
  inferModule m


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right c) = Right c
mapLeft f (Left a)  = Left $ f a
