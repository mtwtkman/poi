module Poi.Action.Move where

import Poi.Entity
import System.Directory
import System.FilePath
import Poi.Action.UpdateMetaInfo
import Poi.Action
import Data.UUID

absoluteObjectPath :: FilePath -> IO ObjectPath
absoluteObjectPath p = MkObjectPath <$> absolutePath p

buildDest :: TrashBox -> MetaInfo -> FilePath
buildDest (MkTrashBox box) m = box </> serialize (unObjectPath m)

trash :: TrashBox -> FilePath -> IO (PoiActionResult ())
trash tb src = do
  absolutePath <- absoluteObjectPath src
  metainfo <- metaInfoFromFilePath src
  renameDirectory src $ buildDest tb metainfo
  result <- addMetaInfoToFile tb metainfo
  case result of
    Right _ -> return $ Right ()
    Left _ -> return $ Left PoiMoveError

back :: TrashBox -> MetaInfo -> IO (PoiActionResult ())
back tb m = do
  let p = unObjectPath m
      uuid = unId m
  renameDirectory (toString uuid) (serialize p)
  result <- deleteMetaInfoFromFile tb m
  case result of
    Right _ -> return $ Right ()
    Left _ -> return $ Left PoiRollbackError
