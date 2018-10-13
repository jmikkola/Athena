module Region where

data Region =
  Region
  { sourceName :: !String
  , start :: !Position
  , end :: !Position
  } deriving (Eq, Ord, Show)

data Position =
  Position
  { line :: !Int
  , column :: !Int
  } deriving (Eq, Ord, Show)
