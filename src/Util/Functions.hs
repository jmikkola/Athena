module Util.Functions where

mapFst :: (a -> b) -> [(a, c)] ->  [(b, c)]
mapFst _ [] = []
mapFst f ((a,b):rest) = (f a, b) : mapFst f rest

mapSnd :: (a -> b) -> [(c, a)] -> [(c, b)]
mapSnd _ [] = []
mapSnd f ((a,b):rest) = (a, f b) : mapSnd f rest

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right c) = Right c
mapLeft f (Left a)  = Left (f a)
