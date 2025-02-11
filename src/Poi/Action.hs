{-# LANGUAGE RankNTypes #-}

module Poi.Action (
  PoiAction (..),
  PoiActionResult,
  PoiCommonError (..),
  PoiBuryError (..),
  PoiActionError (..),
  IndexSpecified,
  listUp,
  toss,
  pickUpByIndex,
  emptyTrashCan,
  deleteTrashesByDayBefore,
  deleteTrashByIndex,
  duplicationSafeName,
  deleteTrashByIndices,
  pickUpByIndices,
  showCurrentVersion,
) where

import Control.Exception (try)
import Control.Monad (forM_, when)
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import qualified Data.Set as S
import Data.Time (
  LocalTime (LocalTime),
  TimeOfDay (TimeOfDay),
  addLocalTime,
  fromGregorian,
  nominalDay,
  secondsToNominalDiffTime,
 )
import qualified Data.UUID as U
import Poi.Abnormal (PoiAbnormal)
import Poi.Display (makeFullPath)
import Poi.Entity (
  OrderedTrashCan (OrderedTrashCan),
  SortOrder (Desc),
  Trash (..),
  TrashCan (TrashCan),
  TrashCanLocation (TrashCanLocation),
  buildTAbsoluteTrashFilePath,
  buildTrashIdPath,
  buildTrashedAtPath,
  sortTrashes,
 )
import Poi.File.IO (
  deleteTrash,
  digTrashCan,
  doesEmptyDirectory,
  doesTrashCanExist,
  trashToCan,
 )
import Safe (atMay)
import System.Directory (
  createDirectoryIfMissing,
  doesPathExist,
  listDirectory,
  removeDirectoryRecursive,
  renamePath,
 )
import System.FilePath (joinPath)
import Poi.Version (version, Version)

data PoiActionError
  = CommonError PoiCommonError
  | PoiBuryError PoiBuryError
  | PoiTossError IOError
  deriving (Show, Eq)

instance PoiAbnormal PoiActionError

data PoiCommonError
  = FileNotFound
  | TrashCanNotFound TrashCanLocation
  | IndexMustBePositive
  | IndexOverFlow
  deriving (Show, Eq)

data PoiBuryError
  = FilePathNotExist
  | BeforeDayMustBeZeroOrPositive
  deriving (Show, Eq)

data PoiAction
  = ListUp
  | Toss [FilePath]
  | PickUpByIndex [Int]
  | EmptyTrashCan
  | DeleteDayBefore Int
  | DeleteByIndex [Int]
  | ShowVersion Version
  deriving (Show, Eq)

type PoiActionResult a = Either PoiActionError a

withTrashCan :: TrashCanLocation -> (TrashCanLocation -> IO (PoiActionResult a)) -> IO (PoiActionResult a)
withTrashCan l a = do
  e <- doesTrashCanExist l
  if e
    then a l
    else return (Left $ CommonError (TrashCanNotFound l))

deleteEmptyTrashedAtPath :: TrashCanLocation -> Trash -> IO Bool
deleteEmptyTrashedAtPath can t = do
  let trashedAtPath = buildTrashedAtPath can (trashedAt t)
  needToClean <- doesEmptyDirectory trashedAtPath
  when needToClean (removeDirectoryRecursive trashedAtPath)
  return needToClean

withIndex :: TrashCanLocation -> Int -> (Trash -> IO a) -> IO (PoiActionResult a)
withIndex l i f
  | i < 0 = return $ Left (CommonError IndexMustBePositive)
  | otherwise = withTrashCan l $ \can -> do
      OrderedTrashCan items <- digTrashCan can <&> sortTrashes Desc
      case atMay items i of
        Just item -> f item <&> Right
        Nothing -> return $ Left (CommonError IndexOverFlow)

listUp :: SortOrder -> TrashCanLocation -> IO (PoiActionResult OrderedTrashCan)
listUp s l = withTrashCan l $ \loc -> do
  ordered <- digTrashCan loc <&> sortTrashes s
  return $ Right ordered

toss :: TrashCanLocation -> [FilePath] -> IO (PoiActionResult [Trash])
toss l fs = do
  res <- try (trashToCan l fs)
  case res of
    Right v -> return $ Right v
    Left e -> return $ Left (PoiTossError e)

type IndexSpecified a = TrashCanLocation -> Int -> IO (PoiActionResult a)

withIndices :: TrashCanLocation -> [Int] -> IndexSpecified a -> IO (PoiActionResult [a])
withIndices can is proc = foldrM f (Right []) is
 where
  f _ acc@(Left _) = return acc
  f i (Right ps) = do
    result <- proc can i
    case result of
      Right p -> return $ Right (p : ps)
      Left e -> return $ Left e

duplicationSafeName :: String -> String
duplicationSafeName = (".poi.pickup." <>)

pickUpByIndex :: IndexSpecified FilePath
pickUpByIndex can i =
  withIndex
    can
    i
    ( \t@(Trash{trashOriginalParentPath = parent, trashId = fid}) -> do
        let origin = makeFullPath t
        src <- buildTAbsoluteTrashFilePath can t
        occupied <- doesPathExist origin
        dest <- do
          if occupied
            then do
              return $ origin <> duplicationSafeName (U.toString fid)
            else
              createDirectoryIfMissing True parent >> return origin
        renamePath src dest
        removeDirectoryRecursive (buildTrashIdPath can t)
        _ <- deleteEmptyTrashedAtPath can t
        return dest
    )

pickUpByIndices :: TrashCanLocation -> [Int] -> IO (PoiActionResult [FilePath])
pickUpByIndices can is = withIndices can is pickUpByIndex

emptyTrashCan :: TrashCanLocation -> IO Int
emptyTrashCan (TrashCanLocation can) = do
  listDirectory can >>= foldrM (\d acc -> removeDirectoryRecursive (joinPath [can, d]) >> return (1 + acc)) 0

deleteTrashByIndex :: IndexSpecified Trash
deleteTrashByIndex can i =
  withIndex
    can
    i
    (\t -> removeDirectoryRecursive (buildTrashIdPath can t) >> deleteEmptyTrashedAtPath can t >> return t)

deleteTrashByIndices :: TrashCanLocation -> [Int] -> IO (PoiActionResult [Trash])
deleteTrashByIndices can is = withIndices can is deleteTrashByIndex

filterByTrashedAt :: (LocalTime, LocalTime) -> TrashCan -> TrashCan
filterByTrashedAt (s, e) (TrashCan trashes) = TrashCan $ S.filter (\(Trash{trashedAt = t}) -> t >= s && t <= e) trashes

deleteTrashesByDayBefore :: TrashCanLocation -> LocalTime -> Int -> IO (PoiActionResult (S.Set Trash))
deleteTrashesByDayBefore can baseDate dayBefore
  | dayBefore < 0 = return $ Left (PoiBuryError BeforeDayMustBeZeroOrPositive)
  | otherwise = do
      trashes <- digTrashCan can
      let TrashCan targets =
            filterByTrashedAt
              ( LocalTime (fromGregorian 1970 1 1) (TimeOfDay 0 0 0)
              , addLocalTime (negate $ nominalDay * secondsToNominalDiffTime (fromInteger $ toInteger dayBefore)) baseDate
              )
              trashes
      forM_ (S.toList targets) (deleteTrash can)
      return $ Right targets

showCurrentVersion :: IO PoiAction
showCurrentVersion = return (ShowVersion version)
