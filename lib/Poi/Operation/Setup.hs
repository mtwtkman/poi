module Poi.Operation.Setup where

import Data.List
import Poi.Entity
import System.Directory
import System.FilePath

createTrashBoxDirectory :: TrashBox -> IO ()
createTrashBoxDirectory (MkTrashBox dest) = createDirectory dest

createMetaInfoFile :: TrashBox -> [MetaInfo] -> IO ()
createMetaInfoFile tb metainfos = do
  let content = intercalate "\n" . map serialize $ metainfos
  writeFile (metaInfoLocation tb) content
