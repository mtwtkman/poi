module Poi.Test.Operation.UpdateMetaInfo (props) where

import System.Random
import Poi.Entity
import Poi.Operation.UpdateMetaInfo
import Poi.Test.Arbitrary
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck

props :: TestTree
props = testGroup "Testing Poi.Operation.UpdateMetaInfo" [prop_addMetaInfo, prop_findMetaInfo]

prop_addMetaInfo =
  testGroup
    "addMetaInfo"
    [ testProperty "added metainfo to last" $
        \ms m -> last (addMetaInfo (ms :: [MetaInfo]) (m :: MetaInfo)) == m
    ]

pickMetaInfo :: RandomGen g => g -> [MetaInfo] -> (MetaInfo, g)
pickMetaInfo rnd xs =
  let len = length xs - 1
      (i, g) = randomR (0, len) rnd
  in (xs !! i, g)

prop_findMetaInfo =
  testGroup
    "findMetaInfo"
    [ testProperty "found it from list" $
        \(NonEmpty ms) -> do
          rnd <- mkStdGen
          let picked = fst $ pickMetaInfo rnd ms
          return $ findMetaInfo ms picked == Just picked
    ]
