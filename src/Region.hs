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


-- TODO: Show what the actual region was
showRegion :: [String] -> Region -> String
showRegion fileLines region =
  let firstLine = line $ start region
      lastLine = line $ end region
      nlines = lastLine - firstLine + 1
      regionLines = take nlines $ drop (firstLine - 1) fileLines
  in unlines regionLines
