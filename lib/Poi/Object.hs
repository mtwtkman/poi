module Poi.Object where

import Control.Monad
import Data.List
import Poi.Entity
import System.Directory
import System.FilePath.Posix (isAbsolute, isRelative, (</>))

moveToTrashBox :: TrashBox -> ObjectPath -> IO ()
moveToTrashBox (MkTrashBox box) (MkObjectPath src) = renameDirectory src $ box </> src

doesTrashBoxExist :: TrashBox -> IO Bool
doesTrashBoxExist (MkTrashBox d) = doesPathExist d

storeDirPath :: TrashBox -> MetaInfo -> FilePath
storeDirPath (MkTrashBox d) m = d </> serialize (getTrashedAt m)

createTrashBoxDirectory :: TrashBox -> IO ()
createTrashBoxDirectory (MkTrashBox d) = do
  existed <- doesPathExist d
  unless existed $ return ()
  createDirectoryIfMissing True d

metaInfoContentDelimiter = "="

buildMetaInfoContent :: MetaInfo -> String
buildMetaInfoContent m = concatLine (map keyValue [("sourcePath", serialize $ getObjectPath m), ("trashedAt", serialize $ getTrashedAt m)])
  where
    keyValue (k, v) = k ++ metaInfoContentDelimiter ++ v
    concatLine = intercalate "\n"

createMetaInfoFile :: FilePath -> MetaInfo -> IO ()
createMetaInfoFile d m = writeFile d (buildMetaInfoContent m)
