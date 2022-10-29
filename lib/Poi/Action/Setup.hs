module Poi.Action.Setup where

import Control.Monad
import Data.List
import Poi.Action.UpdateMetaInfo
import Poi.Entity
import System.Directory
import System.FilePath.Posix

doesTrashBoxExist :: TrashBox -> IO Bool
doesTrashBoxExist (MkTrashBox d) = doesPathExist d

data SetupResult = CreatedTrahsBox | TrashBoxAlreadyExists deriving (Show, Eq)

createTrashBoxDirectory :: TrashBox -> IO SetupResult
createTrashBoxDirectory tb = do
  let (MkTrashBox location) = tb
  existed <- doesPathExist location
  if existed
    then return TrashBoxAlreadyExists
    else do
      createDirectoryIfMissing True location
      createMetaInfoFile tb
      return CreatedTrahsBox
