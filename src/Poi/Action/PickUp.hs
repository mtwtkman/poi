module Poi.Action.PickUp (pickUpByIndex, pickUpByIndices) where

import qualified Data.UUID as U
import Poi.Action.Internal.Context (withIndex, withIndices)
import Poi.Action.Internal.Helper (duplicationSafeName)
import Poi.Action.Type.Index (IndexSpecified)
import Poi.Action.Type.Result (PoiActionResult)
import Poi.Display (makeFullPath)
import Poi.Entity (
  Trash (Trash, trashId, trashOriginalParentPath),
  TrashCanLocation,
  buildTAbsoluteTrashFilePath,
  buildTrashIdPath,
 )
import Poi.File.IO (deleteEmptyTrashedAtPath)
import System.Directory (createDirectoryIfMissing, doesPathExist, removeDirectoryRecursive, renamePath)

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
