module Compiler where

import Errors ( Error(..), Result )
import FirstPass ( firstPass )
import Inference ( inferModule, InferResult )
import Parser ( parseFile )
--import Types ( Scheme )

compile :: String -> Result (InferResult ())
compile text = do
  file <- mapLeft ParseError $ parseFile text
  m <- firstPass file
  inferModule m


mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right c) = Right c
mapLeft f (Left a)  = Left $ f a
