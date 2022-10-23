module Poi.Operation.Move where

import Poi.Entity
import System.Directory
import System.FilePath

absoluteObjectPath :: FilePath -> IO ObjectPath
absoluteObjectPath p = MkObjectPath <$> absolutePath p

moveToTrashBox :: TrashBox -> FilePath -> IO MetaInfo
moveToTrashBox (MkTrashBox box) src = do
  absolutePath <- absoluteObjectPath src
  metainfo <- metaInfoFromFilePath src
  renameDirectory src $ box </> src
  return metainfo
