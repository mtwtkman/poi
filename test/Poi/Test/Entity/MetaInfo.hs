module Poi.Test.Entity.MetaInfo where

import Poi.Test.Arbitrary
import Poi.Entity
import Test.Tasty
import Test.Tasty.SmallCheck as SC

props :: TestTree
props = testGroup "" [prop_Codec]

prop_Codec =
  testGroup
    "Codec"
    [ SC.testProperty "reversible serialize" $
        \x -> (x :: TrashedAt) == x
    ]
