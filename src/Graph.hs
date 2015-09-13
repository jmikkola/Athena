module Graph where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Graph a = Map a (Set a)

emptyGraph :: Graph a
emptyGraph = Map.empty

-- Korasaju's algorithm for finding strongly connected components
-- https://en.wikipedia.org/wiki/Kosaraju's_algorithm
findSCC :: (Ord a) => Graph a -> [Set a]
findSCC graph =
  let nodeStack = collectStack graph (nodeSet graph) []
  in getComponents (transpose graph) nodeStack

collectStack :: (Ord a) => Graph a -> Set a -> [a] -> [a]
collectStack graph remainingNodes stack =
  case setPop remainingNodes of
   Nothing                -> stack
   Just (node, remaining) ->
     let remaining' = Set.difference remaining (reachable graph node)
         (stack', _) = dfsStack graph node (stack, Set.empty)
     in collectStack graph remaining' stack'

dfsStack :: (Ord a) => Graph a -> a -> ([a], Set a) -> ([a], Set a)
dfsStack graph node (stack, seen) =
  if Set.member node seen then (stack, seen)
  else let chldrn = children graph node
           seen' = Set.insert node seen
           (stack', seen'') = foldl (\stSn child -> dfsStack graph child stSn) (stack, seen') chldrn
       in (node : stack', seen'')

-- Set function for convenience
setPop :: Set a -> Maybe (a, Set a)
setPop set =
  if Set.null set then Nothing
  else Just $ Set.deleteFindMin set

getComponents :: (Ord a) => Graph a -> [a] -> [Set a]
getComponents graph nodes = map (reachable graph) nodes

-- Finds all nodes reachable from the given starting node
-- (includes the starting node).
reachable :: (Ord a) => Graph a -> a -> Set a
reachable graph starting = dfs Set.empty starting
  where dfs seen node =
          if Set.member node seen then seen
          else foldl dfs (Set.insert node seen) (children graph node)

children :: (Ord a) => Graph a -> a -> [a]
children graph node = case Map.lookup node graph of
  Nothing -> []
  Just cl -> Set.toList cl

-- Returns the set of all nodes mentioned anywhere in the graph.
nodeSet :: (Ord a) => Graph a -> Set a
nodeSet g = Set.union (Set.fromList $ Map.keys g) referencedNodes
  where referencedNodes = Map.fold Set.union Set.empty g

-- Returns the list of all nodes in the graph.
nodes :: (Ord a) => Graph a -> [a]
nodes = Set.toList . nodeSet

-- Returns the list of all edges in the graph
edges :: Graph a -> [(a, a)]
edges g = Map.foldWithKey collectEdges [] g
  where collectEdge a b es = (a, b) : es
        collectEdges n e es = Set.fold (collectEdge n) es e

-- Reverses the direction of an edge
flipEdge (a, b) = (b, a)

-- Adds an edge (from, to) to the graph.
addEdge :: (Ord a) => Graph a -> (a, a) -> Graph a
addEdge g (v1, v2) = Map.alter ensureEdge v1 g
  where ensureEdge Nothing  = Just $ Set.singleton v2
        ensureEdge (Just e) = Just $ Set.insert v2 e

-- Reverse the direction of all edges in the graph.
transpose :: (Ord a) => Graph a -> Graph a
transpose g =
  let newEdges = map flipEdge $ edges g
      justNodes = Map.fromList [(n, Set.empty) | n <- nodes g]
  in foldl addEdge justNodes newEdges

exampleGraph :: Graph Char
exampleGraph = Map.fromList $
               map (\(v, e) -> (v, Set.fromList e))
               [ ('a', [])  , ('b', "cd") , ('c', []) , ('d', "e")
               , ('e', "f") , ('f', "dg") , ('g', []) , ('h', "ij")
               , ('i', "k") , ('j', "k")  , ('k', []) ]
