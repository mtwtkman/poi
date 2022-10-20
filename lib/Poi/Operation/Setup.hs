module Poi.Operation.Setup where

import Control.Monad
import Data.List
import Poi.Entity
import System.Directory
import System.FilePath.Posix

doesTrashBoxExist :: TrashBox -> IO Bool
doesTrashBoxExist (MkTrashBox d) = doesPathExist d

createMetaInfoFile :: TrashBox -> [MetaInfo] -> IO ()
createMetaInfoFile tb metainfos = do
  let content = intercalate "\n" . map serialize $ metainfos
  writeFile (metaInfoLocation tb) content

createTrashBoxDirectory :: TrashBox -> IO ()
createTrashBoxDirectory (MkTrashBox d) = do
  existed <- doesPathExist d
  unless existed $ return ()
  createDirectoryIfMissing True d
