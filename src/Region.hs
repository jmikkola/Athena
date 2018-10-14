module Region where

data Region =
  Region
  { fileName :: !String
  , start :: !Position
  , end :: !Position
  } deriving (Eq, Ord, Show)

data Position =
  Position
  { line :: !Int
  , column :: !Int
  } deriving (Eq, Ord, Show)
