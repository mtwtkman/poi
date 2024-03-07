module Main where

import qualified Test.Poi.ActionTest as ActionTest
import qualified Test.Poi.Control.Monad.FreeTest as FreeTest
import qualified Test.Poi.Control.Monad.Trans.ReaderTest as ReaderTest
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "poi-core tests"
    [ FreeTest.tests
    , ReaderTest.tests
    , ActionTest.tests
    ]
