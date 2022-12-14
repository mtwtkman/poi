module Main where

import qualified Poi.Test.Action.UpdateMetaInfo as UpdateMetaInfoTest
import qualified Poi.Test.Entity.MetaInfo as MetaInfoTest
import qualified Poi.Test.Entity.TrashedAt as TrashedAtTest
import qualified Poi.Test.Time as TimeTest
import Test.Tasty

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Testing for Poi"
    [ MetaInfoTest.props,
      TimeTest.props,
      TrashedAtTest.props,
      UpdateMetaInfoTest.props
    ]
