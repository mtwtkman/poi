module Poi.Test.Entity.MetaInfo (props) where

import Data.Time.Clock
import Data.Time.Format.ISO8601
import Poi.Entity
import Poi.Test.Arbitrary
import Poi.Time
import Test.SmallCheck.Series
import Test.Tasty
import Test.Tasty.SmallCheck

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
        changeDepth (const 7) $ \m -> serialize (m :: MetaInfo) == ("path=" ++ serialize (getObjectPath m) ++ ",trashed-at=" ++ serialize (getTrashedAt m)),
      testProperty "inversion" $
        changeDepth (const 7) $ \m -> deserialize (serialize (m :: MetaInfo)) == Right m
    ]
