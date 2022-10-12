module Poi.Test.Entity.MetaInfo (props) where

import Poi.Entity
import Poi.Test.Arbitrary
import Poi.Type
import Test.Tasty
import Test.Tasty.SmallCheck as SC

props :: TestTree
props = testGroup "Testing MetaInfo" [prop_Serialize]

prop_Serialize =
  testGroup
    "Serialize"
    [ SC.testProperty "makes formatted string" $
        changeDepth (const 7) $ \m -> serialize (m :: MetaInfo) == ("path=" ++ serialize(getObjectPath m) ++ "\ntrashed-at=" ++ serialize(getTrashedAt m))
    ]
