module Poi.Test.Entity.MetaInfo (props) where

import Data.Time.Clock
import Data.Time.Format.ISO8601
import Poi.Entity
import Poi.Test.Arbitrary
import Poi.Time
import Test.Tasty
import Test.Tasty.QuickCheck

props :: TestTree
props =
  testGroup
    "Testing MetaInfo"
    [ prop_serde
    ]

prop_serde =
  testGroup
    "Serialize"
    [ testProperty "makes formatted string" $
        \m -> serialize (m :: MetaInfo) == ("path=" ++ serialize (getObjectPath m) ++ ",trashed-at=" ++ serialize (getTrashedAt m)),
      testProperty "inversion" $
        \m -> deserialize (serialize (m :: MetaInfo)) == Right m
    ]
