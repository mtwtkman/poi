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
  writeFile (metaInfoFileLocation tb) content

data SetupResult = CreatedTrahsBox | TrashBoxAlreadyExists deriving (Show, Eq)

createTrashBoxDirectory :: TrashBox -> IO SetupResult
createTrashBoxDirectory (MkTrashBox d) = do
  existed <- doesPathExist d
  if existed
    then return TrashBoxAlreadyExists
    else do
      createDirectoryIfMissing True d
      return CreatedTrahsBox

