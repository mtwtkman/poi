module Poi.Action.ListUp (listUp) where

import Data.Functor ((<&>))
import Poi.Action.Internal.Context (withTrashCan)
import Poi.Action.Type.Result (PoiActionResult)
import Poi.Entity (
  OrderedTrashCan,
  SortOrder (..),
  TrashCanLocation,
  sortTrashes,
 )
import Poi.File.IO (digTrashCan)

listUp :: SortOrder -> TrashCanLocation -> IO (PoiActionResult OrderedTrashCan)
listUp s l = withTrashCan l $ \loc -> do
  ordered <- digTrashCan loc <&> sortTrashes s
  return $ Right ordered
