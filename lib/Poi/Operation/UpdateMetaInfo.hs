module Poi.Operation.UpdateMetaInfo where

import Control.Monad
import Data.Maybe
import Poi.Entity
import System.Directory

doesMetaInfoFileExists :: TrashBox -> IO Bool
doesMetaInfoFileExists = doesFileExist . metaInfoFileLocation

data MetaInfoFileAccessError = FileNotFound | MetaInfoFileUpdateError MetaInfoUpdateError deriving (Show, Eq)

type MetaInfoFileAccessResult a = Either MetaInfoFileAccessError a

data MetaInfoUpdateError = UnknownMetaInfo | NoMetaInfo deriving (Show, Eq)

type MetaInfoUpdateResult a = Either MetaInfoUpdateError a

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

currentWholeMetaInfoContext :: TrashBox -> MetaInfo -> ([MetaInfo] -> MetaInfo -> a) -> IO (MetaInfoFileAccessResult a)
currentWholeMetaInfoContext tb m process = do
  contents <- getCurrentWholeMetaInfo tb
  case contents of
    Right metas -> do
      return $ Right (process metas m)
    Left _ -> return (Left FileNotFound)


addMetaInfo :: [MetaInfo] -> MetaInfo -> [MetaInfo]
addMetaInfo metas = (<>) metas . pure

writeMetaInfoFile :: TrashBox -> [MetaInfo] -> IO ()
writeMetaInfoFile tb metas = writeFile (metaInfoFileLocation tb) (unlines $ map serialize metas)

updateMetaInfoFile :: TrashBox -> MetaInfo -> ([MetaInfo] -> MetaInfo -> MetaInfoUpdateResult a) -> IO (MetaInfoFileAccessResult a)
updateMetaInfoFile tb m process = do
  contents <- getCurrentWholeMetaInfo tb
  case contents of
    Right metas -> do
      case process metas m of
        Right result ->  return $ Right result
        Left reason -> return $ Left (MetaInfoFileUpdateError reason)
    Left _ -> return $ Left FileNotFound

addMetaInfoToFile :: TrashBox -> MetaInfo -> IO (MetaInfoFileAccessResult [MetaInfo])
addMetaInfoToFile tb m = do
  contents <- getCurrentWholeMetaInfo tb
  case contents of
    Right metas -> do
      let updated = addMetaInfo metas m
      writeMetaInfoFile tb updated
      return (Right updated)
    Left _ -> return (Left FileNotFound)

findMetaInfo :: [MetaInfo] -> MetaInfo -> Maybe MetaInfo
findMetaInfo (x : xs) m = if x == m then Just x else findMetaInfo xs m
findMetaInfo [] m = Nothing

isKnownMetaInfo :: [MetaInfo] -> MetaInfo -> Bool
isKnownMetaInfo xs x = isJust (findMetaInfo xs x)

deleteMetaInfo :: [MetaInfo] -> MetaInfo -> MetaInfoUpdateResult [MetaInfo]
deleteMetaInfo [] _ = Left NoMetaInfo
deleteMetaInfo xs m = if isKnownMetaInfo xs m then Right [x | x <- xs, x /= m] else Left UnknownMetaInfo

deleteMetaInfoFile :: TrashBox -> MetaInfo -> IO (MetaInfoFileAccessResult [MetaInfo])
deleteMetaInfoFile tb m = do
  content <- getCurrentWholeMetaInfo tb
  case content of
    Right metas -> do
      case deleteMetaInfo metas m of
        Right deleted -> do
          writeMetaInfoFile tb deleted
          return $ Right deleted
        Left reason -> return $ Left (MetaInfoFileUpdateError reason)
    Left _ -> return (Left FileNotFound)
