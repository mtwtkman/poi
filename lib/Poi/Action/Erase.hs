module Poi.Action.Erase where

import Data.Time
import Poi.Entity
import Poi.Action.UpdateMetaInfo
import System.Directory

erase :: TrashBox -> MetaInfo -> IO ()
erase tb m = removeDirectoryRecursive $ trashedObjectPath tb m

eraseDaysBefore :: TrashBox -> Day -> IO ()
eraseDaysBefore tb day = do
  whole <- getCurrentWholeMetaInfo tb
  case whole of
    Right ms -> do
      tz <- getCurrentTimeZone
      let targets = findMetaInfoByDayBefore tz ms day
      mapM_ (erase tb) targets
    Left reason -> fail $ show reason
