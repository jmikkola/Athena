module Main where

import Control.Monad (liftM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), assertFailure )
import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import Graph

main = defaultMain $
       asGroup [ ("strongly connected components", testSCC) ]

exampleGraph1 :: Graph Char
exampleGraph1 = Map.fromList $
               map (\(v, e) -> (v, Set.fromList e))
               [ ('a', [])  , ('b', "cd") , ('c', []) , ('d', "e")
               , ('e', "f") , ('f', "dg") , ('g', []) , ('h', "ij")
               , ('i', "k") , ('j', "k")  , ('k', []) ]

exampleGraph2 :: Graph Char
exampleGraph2 = Map.fromList $
               map (\(v, e) -> (v, Set.fromList e))
               [ ('a', "b"), ('b', "cef"), ('c', "dg"), ('d', "ch")
               , ('e', "af"), ('f', "g"), ('g', "f"), ('h', "gd") ]

testSCC =
  TestList [ ("empty graph" ~: findSCC (emptyGraph :: Graph Char) ~?= [])
           , ("graph 1" ~: findSCC exampleGraph1 ~?=
              (map Set.fromList $ words "h j i k b def g c a"))
           , ("graph 2" ~: findSCC exampleGraph2 ~?=
              (map Set.fromList $ words "eba dch gf")) ]
