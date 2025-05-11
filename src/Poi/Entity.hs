module Poi.Entity (
  Trash (..),
  TrashCan (..),
  TrashCanLocation (..),
  OrderedTrashCan (..),
  SortOrder (..),
  TrashedAtPath,
  sortTrashes,
  doesTrashExist,
  formatTrashedAt,
  buildTrashedAtPath,
  parentFileName,
  trashContainerName,
  defaultPoiTrashCanName,
  trashedAtFormat,
  buildTAbsoluteTrashFilePath,
  buildTrashIdPath,
) where

import Data.Set (Set, member, toAscList, toDescList)
import Data.Time (LocalTime, defaultTimeLocale, formatTime)
import qualified Data.UUID as U
import System.Directory (makeAbsolute)
import System.FilePath (joinPath, splitDrive)

type TrashedAtPath = FilePath

data Trash = Trash
  { trashOriginalPath :: !FilePath
  , trashOriginalParentPath :: !FilePath
  , trashId :: !U.UUID
  , trashedAt :: !LocalTime
  }
  deriving (Show)

instance Eq Trash where
  a == b = trashId a == trashId b

instance Ord Trash where
  (Trash{trashOriginalPath = o1, trashOriginalParentPath = p1, trashId = i1, trashedAt = t1})
    <= (Trash{trashOriginalPath = o2, trashOriginalParentPath = p2, trashId = i2, trashedAt = t2}) = (t1, p1, o1, i1) <= (t2, p2, o2, i2)

newtype TrashCan = TrashCan
  { trashes :: Set Trash
  }
  deriving (Show, Eq, Ord)

newtype TrashCanLocation = TrashCanLocation String
  deriving (Eq)

instance Show TrashCanLocation where
  show (TrashCanLocation s) = s

newtype TimeRecord = TimeRecord LocalTime
  deriving (Show)

data SortOrder = Asc | Desc
  deriving (Eq, Show)

data OrderedTrashCan = OrderedTrashCan
  { orderedTrashes :: [Trash]
  , sortOrder :: SortOrder
  }
  deriving (Eq, Show)

sortTrashes :: SortOrder -> TrashCan -> OrderedTrashCan
sortTrashes order (TrashCan{trashes = t}) =
  OrderedTrashCan
    ( case order of
        Asc -> toAscList t
        Desc -> toDescList t
    )
    order

doesTrashExist :: TrashCan -> Trash -> Maybe Trash
doesTrashExist (TrashCan{trashes = ts}) t = if member t ts then Just t else Nothing

buildTrashIdPath :: TrashCanLocation -> Trash -> FilePath
buildTrashIdPath can (Trash{trashId = fid, trashedAt = tAt}) = joinPath [buildTrashedAtPath can tAt, U.toString fid]

trashedAtFormat :: String
trashedAtFormat = "%FT%H:%M:%S.%q"

formatTrashedAt :: LocalTime -> String
formatTrashedAt = formatTime defaultTimeLocale trashedAtFormat

buildTrashedAtPath :: TrashCanLocation -> LocalTime -> TrashedAtPath
buildTrashedAtPath (TrashCanLocation can) t = joinPath [can, formatTrashedAt t]

buildTAbsoluteTrashFilePath :: TrashCanLocation -> Trash -> IO FilePath
buildTAbsoluteTrashFilePath can (Trash{trashOriginalPath = f, trashId = fid, trashedAt = t}) =
  makeAbsolute $ joinPath [buildTrashedAtPath can t, U.toString fid, trashContainerName, snd $ splitDrive f]

parentFileName :: FilePath
parentFileName = "parent"

trashContainerName :: FilePath
trashContainerName = "trash"

defaultPoiTrashCanName :: String
defaultPoiTrashCanName = ".poi_trash_can"
