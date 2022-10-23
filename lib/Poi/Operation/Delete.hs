module Poi.Operation.Delete where

import Data.Either
import Poi.Entity
import Poi.Operation
import Poi.Operation.UpdateMetaInfo
import System.Directory

deleteObject :: TrashBox -> MetaInfo -> IO ()
deleteObject tb m = do
  undefined

deleteFromTrashBox :: TrashBox -> MetaInfo -> IO (PoiOperationResult ())
deleteFromTrashBox tb m = do
  result <- deleteMetaInfoFromFile tb m
  return $ if isRight result then Right () else Left PoiDeleteError
