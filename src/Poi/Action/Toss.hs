module Poi.Action.Toss (toss) where

import Poi.Action.Type.Result (PoiActionResult, PoiActionError (PoiTossError))
import Poi.Entity (Trash, TrashCanLocation)
import Control.Exception (try)
import Poi.File.IO (trashToCan)
import Poi.Time (getCurrent)

toss :: TrashCanLocation -> [FilePath] -> IO (PoiActionResult [Trash])
toss l fs = do
  current <- getCurrent
  res <- try (trashToCan l fs current)
  case res of
    Right v -> return $ Right v
    Left e -> return $ Left (PoiTossError e)
