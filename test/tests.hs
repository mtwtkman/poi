module Main where

import qualified Poi.Test.E2E as E2ETest
import Test.Tasty

main :: IO ()
main = Test.Tasty.defaultMain tests

tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup
    "Tests"
    [  E2ETest.tests
    ]
