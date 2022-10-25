module Poi.Action.Delete where

import Data.Either
import Poi.Action
import Poi.Action.UpdateMetaInfo
import Poi.Entity
import System.Directory

deleteObject :: TrashBox -> MetaInfo -> IO ()
deleteObject tb m = do
  undefined

deleteFromTrashBox :: TrashBox -> MetaInfo -> IO (PoiActionResult ())
deleteFromTrashBox tb m = do
  result <- deleteMetaInfoFromFile tb m
  return $ if isRight result then Right () else Left PoiDeleteError
