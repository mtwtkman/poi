module Poi.Entity.TrashCan where

import Poi.Entity.TrashedObject (TrashedObject)
import Poi.Config (Config)

data TrashCan = TrashCan
  { trashedObjects :: ![TrashedObject]
  , trashCanConfig :: !Config
  }
  deriving (Show, Eq)
