module Poi.Action.Move where

import Data.UUID
import Poi.Action
import Poi.Action.UpdateMetaInfo
import Poi.Entity
import System.Directory
import System.FilePath

absoluteObjectPath :: FilePath -> IO ObjectPath
absoluteObjectPath p = MkObjectPath <$> absolutePath p

trash :: TrashBox -> FilePath -> IO (PoiActionResult ())
trash tb src = do
  absolutePath <- absoluteObjectPath src
  metainfo <- metaInfoFromFilePath src
  let dest = trashedObjectPath tb metainfo
  createDirectory dest
  renamePath src $ dest </> src
  result <- addMetaInfoToFile tb metainfo
  case result of
    Right _ -> return $ Right ()
    Left _ -> return $ Left PoiMoveError

back :: TrashBox -> MetaInfo -> IO (PoiActionResult ())
back tb m = do
  let p = unObjectPath m
      uuid = unId m
  renamePath (toString uuid) (serialize p)
  result <- deleteMetaInfoFromFile tb m
  case result of
    Right _ -> return $ Right ()
    Left _ -> return $ Left PoiRollbackError
