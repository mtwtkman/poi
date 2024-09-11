module Poi.Entity (
  Trash (..),
  TrashCan (..),
  TrashCanLocation (..),
  OrderedTrashCan (..),
  SortOrder (..),
  TrashedAtPath (..),
  sortTrashes,
  doesTrashExist,
  formatTrashedAt,
  buildTrashedAtPath,
  parentFileName,
  trashContainerName,
  defaultPoiTrashCanName,
  trashedAtFormat,
  buildTrashedAbsPath,
) where

import Data.Set (Set, member, toAscList, toDescList)
import Data.Time (LocalTime, defaultTimeLocale, formatTime)
import qualified Data.UUID as U
import System.FilePath (joinPath, splitDrive)

newtype TrashedAtPath = TrashedAtPath FilePath deriving (Show, Eq, Ord)

data Trash = Trash
  { trashOriginalPath :: !FilePath
  , trashOriginalParentPath :: !FilePath
  , trashId :: !U.UUID
  , trashedAt :: !LocalTime
  }
  deriving (Show, Eq)

instance Ord Trash where
  (Trash{trashOriginalPath = o1, trashOriginalParentPath = p1, trashId = i1, trashedAt = t1})
    <= (Trash{trashOriginalPath = o2, trashOriginalParentPath = p2, trashId = i2, trashedAt = t2}) = (t1, p1, o1, i1) <= (t2, p2, o2, i2)

newtype TrashCan = TrashCan
  { trashes :: Set Trash
  }
  deriving (Show, Eq, Ord)

newtype TrashCanLocation = TrashCanLocation String
  deriving (Show, Eq)

newtype TimeRecord = TimeRecord LocalTime
  deriving (Show)

data SortOrder = Asc | Desc

newtype OrderedTrashCan = OrderedTrashCan [Trash]
  deriving (Show)

sortTrashes :: SortOrder -> TrashCan -> OrderedTrashCan
sortTrashes order (TrashCan{trashes = t}) =
  OrderedTrashCan $
    case order of
      Asc -> toAscList t
      Desc -> toDescList t

doesTrashExist :: TrashCan -> Trash -> Maybe Trash
doesTrashExist (TrashCan{trashes = ts}) t = if member t ts then Just t else Nothing

trashedAtFormat :: String
trashedAtFormat = "%FT%H:%M:%S.%q"

formatTrashedAt :: LocalTime -> String
formatTrashedAt = formatTime defaultTimeLocale trashedAtFormat

buildTrashedAtPath :: TrashCanLocation -> LocalTime -> TrashedAtPath
buildTrashedAtPath (TrashCanLocation can) t = TrashedAtPath $ joinPath [can, formatTrashedAt t]

buildTrashedAbsPath :: TrashCanLocation -> Trash -> FilePath
buildTrashedAbsPath can (Trash{trashOriginalPath = f, trashId = fid, trashedAt = t}) =
  let TrashedAtPath root = buildTrashedAtPath can t
   in joinPath [root, U.toString fid, trashContainerName, snd $ splitDrive f]

parentFileName :: FilePath
parentFileName = "parent"

trashContainerName :: FilePath
trashContainerName = "trash"

defaultPoiTrashCanName :: String
defaultPoiTrashCanName = ".poi_trash_can"
