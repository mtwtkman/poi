module Poi.File.IO (
  findTrashCanLocation,
  doesTrashCanExist,
  reifyTrashes,
  digTrashCan,
  trashToCan,
  saveParentLocation,
  doesEmptyDirectory,
  deleteTrash,
) where

import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import qualified Data.Set as S
import Data.Time (
  getCurrentTime,
  getCurrentTimeZone,
  utcToLocalTime,
 )
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Poi.Entity (
  Trash (..),
  TrashCan (TrashCan),
  TrashCanLocation (TrashCanLocation),
  TrashedAtPath,
  buildTrashedAtPath,
  defaultPoiTrashCanName,
  parentFileName,
  trashContainerName,
 )
import Poi.File.Parser (parseTrashedFilePath)
import Safe (lastMay)
import System.Directory (
  createDirectoryIfMissing,
  doesDirectoryExist,
  doesPathExist,
  getHomeDirectory,
  listDirectory,
  makeAbsolute,
  removeDirectoryRecursive,
  renamePath,
 )
import System.Environment (lookupEnv)
import System.FilePath (joinPath, splitPath, takeDirectory)

findTrashCanLocation :: IO TrashCanLocation
findTrashCanLocation = do
  userSpecified <- lookupEnv "POI_TRASH_CAN_PATH"
  case userSpecified of
    Just l@(_ : _) -> do
      e <- doesDirectoryExist l
      if e
        then TrashCanLocation <$> makeAbsolute l
        else ioError $ userError (l <> " NOT EXIST")
    _ -> getHomeDirectory <&> TrashCanLocation . flip (<>) defaultPoiTrashCanName

doesTrashCanExist :: TrashCanLocation -> IO Bool
doesTrashCanExist (TrashCanLocation p) = doesDirectoryExist p

reifyTrashes :: TrashCanLocation -> [FilePath] -> IO [Trash]
reifyTrashes can = foldrM (\t acc -> parseTrashedFilePath can t <&> (<> acc)) []

digTrashCan :: TrashCanLocation -> IO TrashCan
digTrashCan t@(TrashCanLocation can) =
  (listDirectory can >>= reifyTrashes t) <&> (TrashCan . S.fromList)

saveParentLocation :: TrashedAtPath -> FilePath -> IO ()
saveParentLocation trashedAtPath = writeFile (joinPath [trashedAtPath, parentFileName])

oneTrashToCan :: TrashCanLocation -> FilePath -> IO Trash
oneTrashToCan can src = do
  absSrc <- makeAbsolute src
  fx <- doesPathExist absSrc
  if not fx
    then ioError (userError "FileNotExist")
    else do
      tz <- getCurrentTimeZone
      utc <- getCurrentTime
      fid <- U.nextRandom
      let current = utcToLocalTime tz utc
          trashedAtPath = buildTrashedAtPath can current
          container = joinPath [trashedAtPath, U.toString fid]
          srcParent = takeDirectory absSrc
          trashDest = joinPath [container, trashContainerName]
      case lastMay (splitPath absSrc) of
        Just srcName -> do
          createDirectoryIfMissing True trashDest
          saveParentLocation container srcParent
          renamePath absSrc (joinPath [trashDest, srcName])
          return $ Trash absSrc srcParent fid current
        Nothing -> ioError $ userError "Cannot detect file"

trashToCan :: TrashCanLocation -> [FilePath] -> IO [Trash]
trashToCan can = mapM (oneTrashToCan can)

doesEmptyDirectory :: FilePath -> IO Bool
doesEmptyDirectory p = listDirectory p <&> null

deleteTrash :: TrashCanLocation -> Trash -> IO ()
deleteTrash can (Trash{trashedAt = t}) = removeDirectoryRecursive (buildTrashedAtPath can t)
