module Util.Graph (
  Graph,
  components,
  nodes,
  children
  ) where

import Control.Monad.State (State, modify, get, evalState)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph a = Map a [a]

nodes :: Graph a -> [a]
nodes = Map.keys

children :: (Ord a) => Graph a -> a -> [a]
children graph node =
  case Map.lookup node graph of
   Nothing -> []
   Just ch -> ch


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
  lowlink <- Map.lookup node <$> sccIndexes <$> get
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
-- TODO: Add a quickcheck test to make sure that this returns
-- them in topological order in the right direction
