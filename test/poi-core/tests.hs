module Main where

import qualified Test.Poi.Control.Monad.FreeTest as FreeTest
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "poi-core tests"
    [ FreeTest.tests
    ]
