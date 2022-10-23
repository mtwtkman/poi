module Poi.Operation.Move where

import Poi.Entity
import System.Directory
import System.FilePath
import Poi.Operation.UpdateMetaInfo
import Poi.Operation
import Data.UUID

absoluteObjectPath :: FilePath -> IO ObjectPath
absoluteObjectPath p = MkObjectPath <$> absolutePath p

buildDest :: TrashBox -> MetaInfo -> FilePath
buildDest (MkTrashBox box) m = box </> serialize (unObjectPath m)

moveToTrashBox :: TrashBox -> FilePath -> IO (PoiOperationResult ())
moveToTrashBox tb src = do
  absolutePath <- absoluteObjectPath src
  metainfo <- metaInfoFromFilePath src
  renameDirectory src $ buildDest tb metainfo
  result <- addMetaInfoToFile tb metainfo
  case result of
    Right _ -> return $ Right ()
    Left _ -> return $ Left PoiMoveError

rollback :: TrashBox -> MetaInfo -> IO (PoiOperationResult ())
rollback tb m = do
  let p = unObjectPath m
      uuid = unId m
  renameDirectory (toString uuid) (serialize p)
  result <- deleteMetaInfoFromFile tb m
  case result of
    Right _ -> return $ Right ()
    Left _ -> return $ Left PoiRollbackError
