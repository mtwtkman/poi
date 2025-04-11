module Poi.Action.Type.Index (IndexSpecified) where

import Poi.Action.Type.Result (PoiActionResult)
import Poi.Entity (TrashCanLocation)

type IndexSpecified a = TrashCanLocation -> Int -> IO (PoiActionResult a)
