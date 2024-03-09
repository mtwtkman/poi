module Test.Poi.ActionTest (tests) where

import Test.Tasty (TestTree, testGroup)

tests :: TestTree
tests =
  testGroup
    "Poi.Action"
    [ prop_moveFile
    ]

prop_moveFile :: TestTree
prop_moveFile =
  testGroup
    "moveFile spec"
    []
