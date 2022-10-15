module Poi.Entity
  ( ObjectPath (..),
    absoluteObjectPath,
    TrashedAt (..),
    trashedAtToLocalTime,
    MetaInfo (..),
    TrashBox (..),
  )
where

import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Poi.Time
import Poi.Type
import System.FilePath

newtype ObjectPath = MkObjectPath FilePath deriving (Show, Eq)

instance Serialize ObjectPath where
  serialize (MkObjectPath o) = show o

absoluteObjectPath :: FilePath -> FilePath -> ObjectPath
absoluteObjectPath d p
  | isAbsolute p = MkObjectPath p
  | otherwise = MkObjectPath (d </> p)

newtype TrashedAt = MkTrashedAt Timestamp deriving (Show, Eq)

instance Serialize TrashedAt where
  serialize (MkTrashedAt t) = iso8601Show $ timestampToUTCTime t

instance Deserialize TrashedAt where
  deserialize s = do
    value <- iso8601ParseM s
    return $ MkTrashedAt (utcTimeToTimestamp value)

trashedAtToLocalTime :: TrashedAt -> IO LocalTime
trashedAtToLocalTime (MkTrashedAt t) = do
  tz <- getCurrentTimeZone
  return $ timestampToLocalTime tz t

data MetaInfo = MkMetaInfo
  { getObjectPath :: ObjectPath,
    getTrashedAt :: TrashedAt
  }
  deriving (Show, Eq)

instance Serialize MetaInfo where
  serialize a = intercalate "\n" $ map (\(k, v) -> k ++ "=" ++ v) [("path", serialize $ getObjectPath a), ("trashed-at", serialize $ getTrashedAt a)]

instance Deserialize MetaInfo where
  deserialize s = undefined

newtype TrashBox = MkTrashBox FilePath deriving (Show)
