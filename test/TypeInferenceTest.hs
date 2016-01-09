module Main where

import Control.Monad (liftM)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.HUnit ( Assertion, Test(..), (~:), (~?=), assertFailure )
import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import TypeInference

main = defaultMain $
       asGroup [ ("result", testInfResult) ]

emptyResult :: InfResult
emptyResult = (Map.empty, Map.empty)

testInfResult =
  TestList [ "missing type var" ~: getTypeForVar emptyResult 1 ~?= Nothing ]
