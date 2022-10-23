module Poi.Test.Operation.UpdateMetaInfo (props) where

import Data.Maybe
import Poi.Entity
import Poi.Operation.UpdateMetaInfo
import Poi.Test.Arbitrary
import System.Random
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

props :: TestTree
props = testGroup "Testing Poi.Operation.UpdateMetaInfo" [prop_addMetaInfo, prop_findMetaInfo, prop_deleteMetaInfo]

prop_addMetaInfo =
  testGroup
    "addMetaInfo"
    [ testProperty "added metainfo to last" $
        \ms m -> (last <$> addMetaInfo (ms :: [MetaInfo]) (m :: MetaInfo)) == Right m
    ]

prop_findMetaInfo =
  testGroup
    "findMetaInfo"
    [ testProperty "found it from list" $
        \(NonEmpty ms) -> do
          rnd <- mkStdGen
          let picked = fst $ pickMetaInfo rnd ms
          return $ findMetaInfo ms picked == Just picked,
      testProperty "cannot find it from list" $
        \ms m -> isNothing (findMetaInfo (ms :: [MetaInfo]) (m :: MetaInfo))
    ]

prop_deleteMetaInfo =
  testGroup
    "deleteMetaInfo"
    [ testProperty "deleted it from list" $
        \(NonEmpty ms) -> do
          rnd <- mkStdGen
          let picked = fst $ pickMetaInfo rnd ms
          return $ deleteMetaInfo ms picked == Right (takeWhile (/= picked) ms ++ tail (dropWhile (/= picked) ms))
    ]
