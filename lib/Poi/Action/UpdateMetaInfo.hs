module Poi.Action.UpdateMetaInfo where

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

addMetaInfo :: [MetaInfo] -> MetaInfo -> MetaInfoUpdateResult [MetaInfo]
addMetaInfo metas = Right . (<>) metas . pure

writeMetaInfoFile :: TrashBox -> [MetaInfo] -> IO ()
writeMetaInfoFile tb metas = writeFile (metaInfoFileLocation tb) (unlines $ map serialize metas)

updateMetaInfoFile :: TrashBox -> MetaInfo -> ([MetaInfo] -> MetaInfo -> MetaInfoUpdateResult [MetaInfo]) -> IO (MetaInfoFileAccessResult [MetaInfo])
updateMetaInfoFile tb m process = do
  contents <- getCurrentWholeMetaInfo tb
  case contents of
    Right metas -> do
      case process metas m of
        Right newValues -> do
          writeMetaInfoFile tb newValues
          return $ Right newValues
        Left reason -> return $ Left (MetaInfoFileUpdateError reason)
    Left _ -> return $ Left FileNotFound

addMetaInfoToFile :: TrashBox -> MetaInfo -> IO (MetaInfoFileAccessResult [MetaInfo])
addMetaInfoToFile tb m = updateMetaInfoFile tb m addMetaInfo

findMetaInfo :: [MetaInfo] -> MetaInfo -> Maybe MetaInfo
findMetaInfo (x : xs) m = if x == m then Just x else findMetaInfo xs m
findMetaInfo [] m = Nothing

isKnownMetaInfo :: [MetaInfo] -> MetaInfo -> Bool
isKnownMetaInfo xs x = isJust (findMetaInfo xs x)

deleteMetaInfo :: [MetaInfo] -> MetaInfo -> MetaInfoUpdateResult [MetaInfo]
deleteMetaInfo [] _ = Left NoMetaInfo
deleteMetaInfo xs m = if isKnownMetaInfo xs m then Right [x | x <- xs, x /= m] else Left UnknownMetaInfo

deleteMetaInfoFromFile :: TrashBox -> MetaInfo -> IO (MetaInfoFileAccessResult [MetaInfo])
deleteMetaInfoFromFile tb m = updateMetaInfoFile tb m deleteMetaInfo
