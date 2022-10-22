module Poi.Test.Time (props) where

import Data.Time.Clock
import Poi.Test.Arbitrary
import Poi.Time
import Test.Tasty
import Test.Tasty.QuickCheck

props :: TestTree
props = testGroup "Testing Poi.Time" [prop_Conversions]

prop_Conversions =
  testGroup
    "Conversions"
    [ testProperty "can converts utc time to picoseconds also opposite" $
        \t -> timestampToUTCTime (utcTimeToTimestamp (t :: UTCTime)) == t
    ]
