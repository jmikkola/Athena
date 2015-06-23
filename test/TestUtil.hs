module TestUtil where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)

asGroup namedTests = map convert namedTests
  where convert (name, fn) = testGroup name (hUnitTestToTests fn)
