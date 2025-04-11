module Poi.Action.Bury (
  emptyTrashCan,
  deleteTrashByIndex,
  deleteTrashByIndices,
  deleteTrashesByDayBefore,
) where

import Control.Monad (forM_)
import Data.Foldable (foldrM)
import qualified Data.Set as S
import Data.Time (LocalTime (LocalTime), TimeOfDay (TimeOfDay), addLocalTime, fromGregorian, nominalDay, secondsToNominalDiffTime)
import Poi.Action.Internal.Context (withIndex, withIndices)
import Poi.Action.Type.Index (IndexSpecified)
import Poi.Action.Type.Result (PoiActionError (PoiBuryError), PoiActionResult, PoiBuryError (BeforeDayMustBeZeroOrPositive))
import Poi.Entity (Trash (Trash, trashedAt), TrashCan (TrashCan), TrashCanLocation (TrashCanLocation), buildTrashIdPath)
import Poi.File.IO (deleteEmptyTrashedAtPath, deleteTrash, digTrashCan)
import System.Directory (listDirectory, removeDirectoryRecursive)
import System.FilePath (joinPath)

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
