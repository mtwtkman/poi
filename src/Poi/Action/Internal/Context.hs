module Poi.Action.Internal.Context (
  withTrashCan,
  withIndex,
  withIndices,
) where

import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Poi.Action.Type.Index (IndexSpecified)
import Poi.Action.Type.Result (
  PoiActionError (CommonError),
  PoiActionResult,
  PoiCommonError (IndexMustBePositive, IndexOverFlow, TrashCanNotFound),
 )
import Poi.Entity (OrderedTrashCan (OrderedTrashCan), SortOrder (Desc), Trash, TrashCanLocation, sortTrashes)
import Poi.File.IO (digTrashCan, doesTrashCanExist)
import Safe (atMay)

withTrashCan :: TrashCanLocation -> (TrashCanLocation -> IO (PoiActionResult a)) -> IO (PoiActionResult a)
withTrashCan l a = do
  e <- doesTrashCanExist l
  if e
    then a l
    else return (Left $ CommonError (TrashCanNotFound l))

withIndex :: TrashCanLocation -> Int -> (Trash -> IO a) -> IO (PoiActionResult a)
withIndex l i f
  | i < 0 = return $ Left (CommonError IndexMustBePositive)
  | otherwise = withTrashCan l $ \can -> do
      OrderedTrashCan items <- digTrashCan can <&> sortTrashes Desc
      case atMay items i of
        Just item -> f item <&> Right
        Nothing -> return $ Left (CommonError IndexOverFlow)

withIndices :: TrashCanLocation -> [Int] -> IndexSpecified a -> IO (PoiActionResult [a])
withIndices can is proc = foldrM f (Right []) is
 where
  f _ acc@(Left _) = return acc
  f i (Right ps) = do
    result <- proc can i
    case result of
      Right p -> return $ Right (p : ps)
      Left e -> return $ Left e
