module Poi.Test.E2E (tests) where

import qualified Poi.Test.E2E.File as FileTest
import qualified Poi.Test.E2E.Action as ActionTest
import Test.Tasty (TestTree)
import Test.Tasty.Runners (TestTree (TestGroup))

tests :: TestTree
tests =
  TestGroup
    "E2E tests"
    [ FileTest.tests
    , ActionTest.tests
    ]
