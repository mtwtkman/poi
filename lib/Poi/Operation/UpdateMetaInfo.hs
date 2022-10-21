module Poi.Operation.UpdateMetaInfo where

import Control.Monad
import Poi.Entity
import System.Directory

doesMetaInfoFileExists :: TrashBox -> IO Bool
doesMetaInfoFileExists = doesFileExist . metaInfoFileLocation

data MetaInfoFileAccessError = FileNotFound deriving (Show, Eq)

type MetaInfoFileAccessResult a = Either MetaInfoFileAccessError a

getCurrentWholeMetaInfo :: TrashBox -> IO (MetaInfoFileAccessResult [MetaInfo])
getCurrentWholeMetaInfo trashbox = do
  exists <- doesMetaInfoFileExists trashbox
  if exists
    then do
      content <- readFile $ metaInfoFileLocation trashbox
      let parsed = mapM deserialize $ lines content :: DeserializeResult [MetaInfo]
      case parsed of
        Right metainfos -> return (Right metainfos)
        Left DeserializeFailed -> return (Left FileNotFound)
    else return (Left FileNotFound)

updateMetaInfoFile :: TrashBox -> MetaInfo -> IO (MetaInfoFileAccessResult [MetaInfo])
updateMetaInfoFile tb m = do
  contents <- getCurrentWholeMetaInfo tb
  case contents of
    Right metas -> do
      let updated = metas ++ [m]
      writeFile (metaInfoFileLocation tb) (unlines $ map serialize updated)
      return (Right updated)
    Left _ -> return (Left FileNotFound)
