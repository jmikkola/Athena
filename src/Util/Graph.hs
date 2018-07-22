module Util.Graph (
  Graph,
  components,
  nodes,
  children,
  test
  ) where

import Control.Monad.State (State, modify, get, evalState)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Test.QuickCheck

type Graph a = Map a [a]

nodes :: Graph a -> [a]
nodes = Map.keys

children :: (Ord a) => Graph a -> a -> [a]
children graph node =
  case Map.lookup node graph of
   Nothing -> []
   Just ch -> ch


reachable :: (Ord a) => a -> Graph a -> Set a
reachable node graph = findReachable (children graph node) (Set.singleton node)
  where findReachable []     seen = seen
        findReachable (c:cs) seen =
          if Set.member c seen
          then findReachable cs seen
          else findReachable (children graph c ++ cs) (Set.insert c seen)


-- This finds strongly-connected components of the graph,
-- and returns them in a topological ordering.
-- This assumes that every node is mentioned as a key in the graph.
-- https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm
components :: (Ord a) => Graph a -> [[a]]
components graph = evalState runConnect startingSCCState
  where startingSCCState = SCCState
                           { sccIndex = 0
                           , sccLowlinks = Map.empty
                           , sccIndexes = Map.empty
                           , sccComponents = []
                           , sccStack = []
                           , sccInStack = Set.empty }
        runConnect = connect (nodes graph) graph

data SCCState a
  = SCCState
    { sccIndex      :: Int
    , sccLowlinks   :: Map a Int
    , sccIndexes    :: Map a Int
    , sccComponents :: [[a]]
    , sccStack      :: [a]
    , sccInStack    :: Set a
    }

connect :: (Ord a) => [a] -> Graph a -> State (SCCState a) [[a]]
connect [] _ = do
  state <- get
  return $ sccComponents state
connect (n:ns) graph = do
  exists <- isIndexed n
  if exists
    then connect ns graph
    else do
      strongConnect n graph
      connect ns graph

strongConnect :: (Ord a) => a -> Graph a -> State (SCCState a) ()
strongConnect node graph = do
  idx <- sccIndex <$> get
  modify $ setIndex node idx
  modify $ setLowlink node idx
  modify $ incIndex
  modify $ pushStack node

  connectChildren (children graph node) node graph
  gatherComponent node

connectChildren :: (Ord a) => [a] -> a -> Graph a -> State (SCCState a) ()
connectChildren []     _    _     = return ()
connectChildren (c:cs) node graph = do
  exists <- isIndexed c
  isOnStack <- (Set.member c . sccInStack) <$> get
  if not exists
    then do
      strongConnect c graph
      vll <- getLowlink node <$> get
      cll <- getLowlink c <$> get
      modify $ setLowlink node (min vll cll)
    else if isOnStack
         then do
           vll <- getLowlink node <$> get
           cidx <- getIndex c <$> get
           modify $ setLowlink node (min vll cidx)
         else return ()
  connectChildren cs node graph

gatherComponent :: (Ord a) => a -> State (SCCState a) ()
gatherComponent node = do
  index <- Map.lookup node <$> sccIndexes <$> get
  lowlink <- Map.lookup node <$> sccLowlinks <$> get
  if lowlink == index
    then do
      component <- popComponent node
      modify $ \sccs -> sccs { sccComponents = component : (sccComponents sccs) }
    else return ()

popComponent :: (Ord a) => a -> State (SCCState a) [a]
popComponent node = do
  w <- popStack
  if w == node
    then return [w]
    else do
      rest <- popComponent node
      return $ w : rest

type SCCStateTX a = SCCState a -> SCCState a

isIndexed :: (Ord a) => a -> State (SCCState a) Bool
isIndexed node = do
  indexes <- sccIndexes <$> get
  return $ Map.member node indexes

pushStack :: (Ord a) => a -> SCCStateTX a
pushStack node sccs =
  sccs { sccStack = node : (sccStack sccs)
       , sccInStack = Set.insert node (sccInStack sccs) }

popStack :: (Ord a) => State (SCCState a) a
popStack = do
  stack <- sccStack <$> get
  let item = head stack
  modify $ \scss ->
    scss { sccStack = tail stack
         , sccInStack = Set.delete item (sccInStack scss) }
  return item

incIndex :: SCCStateTX a
incIndex sccs = sccs { sccIndex = 1 + sccIndex sccs }

setIndex :: (Ord a) => a -> Int -> SCCStateTX a
setIndex node index sccs =
  sccs { sccIndexes = Map.insert node index (sccIndexes sccs) }

setLowlink :: (Ord a) => a -> Int -> SCCStateTX a
setLowlink node lowlink sccs =
  sccs { sccLowlinks = Map.insert node lowlink (sccLowlinks sccs) }

getIndex :: (Ord a) => a -> SCCState a -> Int
getIndex node sccs = fromJust $ Map.lookup node $ sccIndexes sccs

getLowlink :: (Ord a) => a -> SCCState a -> Int
getLowlink node sccs = fromJust $ Map.lookup node $ sccLowlinks sccs

fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing  = error "unexpected Nothing"

test :: IO ()
test = do
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
