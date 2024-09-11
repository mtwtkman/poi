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
) where

import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.Time (LocalTime)
import qualified Data.UUID as U
import Poi.Display (makeFullPath)
import Poi.Entity (
  OrderedTrashCan (OrderedTrashCan),
  SortOrder (Desc),
  Trash (..),
  TrashCanLocation (TrashCanLocation),
  buildTrashedAbsPath,
  buildTrashedAtPath,
  sortTrashes, TrashedAtPath (TrashedAtPath),
 )
import Poi.File.IO (
  digTrashCan,
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
import Prelude hiding (lookup)

data PoiActionError
  = CommonError PoiCommonError
  | PoiBuryError PoiBuryError
  deriving (Show, Eq)

data PoiCommonError
  = FileNotFound
  | IndexMustBePositive
  | IndexOverFlow
  deriving (Show, Eq)

data PoiBuryError
  = FilePathNotExist
  | BeforeDayMustBeZeroOrPositive
  deriving (Show, Eq)

data PoiAction
  = ListUp
  | Toss FilePath
  | PickUpByIndex Int
  | EmptyTrashCan
  | DeleteDayBefore Int
  | DeleteByIndex Int
  deriving (Show, Eq)

type PoiActionResult a = Either PoiActionError a

withTrashCan :: TrashCanLocation -> IO a -> IO a
withTrashCan l a = do
  e <- doesTrashCanExist l
  if e
    then a
    else error "out"

withIndex :: TrashCanLocation -> Int -> PoiActionError -> (Trash -> IO a) -> IO (PoiActionResult a)
withIndex can i e f
  | i < 0 = return $ Left (CommonError IndexMustBePositive)
  | otherwise = withTrashCan can $ do
      OrderedTrashCan items <- listUp can
      case atMay items i of
        Just item -> f item <&> Right
        Nothing -> return $ Left e

listUp :: TrashCanLocation -> IO OrderedTrashCan
listUp l = withTrashCan l $ digTrashCan l <&> sortTrashes Desc

toss :: TrashCanLocation -> [FilePath] -> IO [Trash]
toss l fs = withTrashCan l $ trashToCan l fs

type IndexSpecified a = TrashCanLocation -> Int -> IO (PoiActionResult a)

duplicateSafeName :: String -> String
duplicateSafeName = (".poi.pickup." <>)

pickUpByIndex :: IndexSpecified FilePath
pickUpByIndex can i =
  withIndex
    can
    i
    (CommonError IndexOverFlow)
    ( \t@(Trash{trashOriginalParentPath = parent, trashId = fid, trashedAt = tAt}) -> do
        let origin = makeFullPath t
            src = buildTrashedAbsPath can t
        occupied <- doesPathExist origin
        dest <- do
          if occupied
            then do
              return $ origin <> duplicateSafeName (U.toString fid)
            else
              createDirectoryIfMissing True parent >> return origin
        renamePath src dest
        let TrashedAtPath trashedAtPath = buildTrashedAtPath can tAt
        removeDirectoryRecursive trashedAtPath
        return dest
    )

emptyTrashCan :: TrashCanLocation -> IO Int
emptyTrashCan (TrashCanLocation can) = do
  listDirectory can >>= foldrM (\d acc -> removeDirectoryRecursive (joinPath [can, d]) >> return (1 + acc)) 0

deleteTrashByIndex :: IndexSpecified Trash
deleteTrashByIndex can i = undefined

deleteTrashesByDayBefore :: TrashCanLocation -> LocalTime -> Int -> IO (PoiActionResult [Trash])
deleteTrashesByDayBefore can pivot d = undefined
