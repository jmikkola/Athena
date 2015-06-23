module Main where

import Test.Framework (defaultMain)

import TestUtil ( asGroup )

import Parse
import Eval

main = defaultMain $
       asGroup [
               ]
