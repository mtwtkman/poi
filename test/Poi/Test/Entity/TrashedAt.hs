module Poi.Test.Entity.TrashedAt (props) where

import Poi.Entity
import Poi.Type
import Test.Tasty
import Test.Tasty.SmallCheck
import Poi.Test.Arbitrary

props :: TestTree
props = testGroup "Testing TrashedAt" [prop_Deserialize]

prop_Deserialize =
  testGroup
    "Deserialize"
    [ testProperty "builds from string" $
        changeDepth (const 6) $ \t -> deserialize (serialize (t :: TrashedAt)) == Right t
    , testProperty "cannot build against malformed string" $
        \s -> (deserialize ("NOT ISO8601 FORMAT! " ++ s :: String) :: DeserializeResult TrashedAt)== Left DeserializeFailed
    ]
