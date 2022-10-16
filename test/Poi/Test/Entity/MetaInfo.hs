module Poi.Test.Entity.MetaInfo (props) where

import Poi.Entity
import Poi.Test.Arbitrary
import Poi.Type
import Poi.Time
import Data.Time.Clock
import Data.Time.Format.ISO8601
import Test.Tasty
import Test.Tasty.SmallCheck

props :: TestTree
props = testGroup "Testing MetaInfo" [prop_Serialize]

prop_Serialize =
  testGroup
    "Serialize"
    [ testProperty "makes formatted string" $
        changeDepth (const 7) $ \m -> serialize (m :: MetaInfo) == ("path=" ++ serialize(getObjectPath m) ++ "\ntrashed-at=" ++ serialize(getTrashedAt m))
    ]

prop_Deserialize =
  testGroup
    "Deserialize"
    [ testProperty "builds from string " $
        \objectPath utc -> let s = "path=" ++ (objectPath :: String) ++ "\ntrashed-at=" ++ iso8601Show (utc :: UTCTime)
                            in deserialize s == Right (MkMetaInfo (MkObjectPath objectPath) (MkTrashedAt (utcTimeToTimestamp utc)))
    , testProperty "cannot build from malformed" $
        \s -> (deserialize s :: DeserializeResult MetaInfo) == Left DeserializeFailed
    ]
