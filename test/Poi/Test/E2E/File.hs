module Poi.Test.E2E.File (tests) where

import qualified Poi.Test.E2E.File.IO as IOTest
import Test.Tasty (TestTree)
import Test.Tasty.Runners (TestTree (TestGroup))

tests :: TestTree
tests =
  TestGroup
    "Poi.File e2e tests"
    [ IOTest.tests
    ]
