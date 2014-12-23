
-- Basically haskell syntax now

module routing (Routes, router, addRoute, matchRoute) where

type PatternName = String

data Routes = Routes { namesUsed: Set PatternName
                     , routes: [(Regex, PatternName)]
                     }
router :: Routes
router = Routes Set.empty []

addRoute :: Routes -> Regex -> String -> Either Error Routes
addRoute Routes(namesUsed, routes) pattern name =
  if contains (namesUsed routes) name
  then Left $ "Duplicate pattern name: " ++ name
  else Right (add namesUsed route) ((pattern, name) : routes)

matchRoute :: Routes -> String -> Maybe String
matchRoute Route(namesUsed, routes) url = matchAny routes url

matchAny :: [(Regex, PatternName)] -> Url -> Maybe PatternName
matchAny []                     _   = Nothing
matchAny ((pattern, name):rest) url = if matches pattern url
                                      then Just name
                                      else matchAny rest url
