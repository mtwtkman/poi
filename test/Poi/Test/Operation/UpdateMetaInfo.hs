module Poi.Test.Operation.UpdateMetaInfo (props) where

import Poi.Entity
import Poi.Operation.UpdateMetaInfo
import Poi.Test.Arbitrary
import Test.Tasty
import Test.Tasty.SmallCheck

props :: TestTree
props = testGroup "Testing Poi.Operation.UpdateMetaInfo" [prop_addMetaInfo, prop_findMetaInfo]

prop_addMetaInfo =
  testGroup
    "addMetaInfo"
    [ testProperty "added metainfo to last" $
        changeDepth (const 9) $ \ms m -> last (addMetaInfo (ms :: [MetaInfo]) (m :: MetaInfo)) == m
    ]

sampleItem :: [MetaInfo] -> MetaInfo
sampleItem metas = undefined

prop_findMetaInfo =
  testGroup
    "findMetaInfo"
    [ testProperty "found it from list" $
        changeDepth (const 9) $ \ms -> let picked = sampleItem ms in findMetaInfo ms picked == Just picked
    ]
