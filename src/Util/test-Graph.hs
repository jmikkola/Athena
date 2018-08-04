module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck

import Util.Graph
import UnitTest
  ( Assertion
  , assertRight
  , assertLeft
  , runTests
  , test )


main = do
  putStrLn "testing graph"
  tests


tests :: IO ()
tests = do
  testExample
  quickCheck propNoEmptyGroups
  quickCheck propSameCardnality
  quickCheck propDisjoint
  quickCheck propAllReachableInGroups
  quickCheck propTopological


propNoEmptyGroups :: Graph Char -> Bool
propNoEmptyGroups graph =
  let cmps = components $ fixupGraph graph
  in all (not . null) cmps

propSameCardnality :: Graph Char -> Bool
propSameCardnality graph =
  let g = fixupGraph graph
      cmps = components g
  in length g == sum (map length cmps)

propDisjoint :: Graph Char -> Bool
propDisjoint graph =
  let ns = concat $ components $ fixupGraph graph
  in length ns == length (Set.toList $ Set.fromList ns)

propAllReachableInGroups :: Graph Char -> Bool
propAllReachableInGroups graph =
  let g = fixupGraph graph
      cmps = components g
      allReachable ns node = let seen = reachable node g in all (\n -> Set.member n seen) ns
      mutuallyReachable ns = all (allReachable ns) ns
  in all mutuallyReachable cmps

propTopological :: Graph Char -> Bool
propTopological graph =
  let g = fixupGraph graph
      cmps = components g
      noBackreferences (previous, group) =
        let prevSet = Set.fromList $ concat previous
            groupReachable = foldl Set.union Set.empty $ map (\n -> reachable n g) group
        in all (\n -> not $ Set.member n prevSet) $ Set.toList groupReachable
  in all noBackreferences (prefixes cmps)


prefixes :: [a] -> [([a], a)]
prefixes items = prefixes' items []
  where prefixes' []     _     = []
        prefixes' (x:xs) trail = (reverse trail, x) : prefixes' xs (x : trail)

-- fixupGraph adds missing nodes
fixupGraph :: (Ord a) => Graph a -> Graph a
fixupGraph graph = Map.union graph empties
  where chs = Set.toList $ foldl Set.union Set.empty $ map Set.fromList $ map snd $ Map.toList graph
        empties = Map.fromList $ zip chs (repeat [])

assertEq :: (Eq a, Show a) => a -> a -> IO ()
assertEq x y =
  if x == y
  then putStrLn "passed"
  else putStrLn (show x ++ " does not equal " ++ show y)

testExample :: IO ()
testExample =
  let graph = Map.fromList [('a', "b"), ('b', "ecf"), ('c', "dg"), ('d', "ch"), ('e', "af"), ('f', "g"), ('g', "f"), ('h', "gd")]
      expected = reverse $ map reverse ["fg", "cdh", "abe"]
  in assertEq expected (components graph)

