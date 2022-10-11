module Poi where

import Control.Monad
import Poi.Entity
import Poi.Object
import Poi.Time
import System.Directory

buildMetaInfo :: Timestamp -> FilePath -> FilePath -> MetaInfo
buildMetaInfo t d src = MkMetaInfo (absoluteObjectPath d src) (MkTrashedAt t)

trash :: TrashBox -> FilePath -> IO ()
trash box f = do
  existed <- doesPathExist f
  unless existed $ return ()
  t <- getCurrentTimestamp
  d <- getCurrentDirectory
  let metainfo = buildMetaInfo t d f
      storeDir = storeDirPath box metainfo
  createDirectoryIfMissing True storeDir
