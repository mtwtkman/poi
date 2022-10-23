module Poi.Test.Entity.MetaInfo (props) where

import Data.Maybe
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Data.UUID
import Poi.Entity
import Poi.Test.Arbitrary
import Poi.Time
import System.Random
import Test.Tasty
import Test.Tasty.QuickCheck

props :: TestTree
props =
  testGroup
    "Testing MetaInfo"
    [ prop_Serde
    ]

prop_Serde =
  testGroup
    "Serialize"
    [ testProperty "makes formatted string" $
        \m -> serialize (m :: MetaInfo) == ("id=" ++ toString (unId m) ++ ",path=" ++ serialize (unObjectPath m) ++ ",trashed-at=" ++ serialize (unTrashedAt m)),
      testProperty "inversion" $
        \m -> deserialize (serialize (m :: MetaInfo)) == Right m
    ]
