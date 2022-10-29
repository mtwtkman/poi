module Poi.Test.Action.List (props) where

import Poi.Action.List
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

props :: TestTree
props = testGroup "Testing Poi.Action.List" [prop_indexedList]

prop_indexedList =
  testGroup
    "indexedList"
    [ testProperty "adds index per element" $
        \i (NonEmpty xs) -> map snd (indexedList i (xs :: [Int])) == xs
    ]
