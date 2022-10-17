{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Poi.Entity where

import Data.List
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Poi.Time
import System.FilePath
import Text.RE.TDFA

class Serialize a where
  serialize :: a -> String

data DeserializeError = DeserializeFailed deriving (Show, Eq)

type DeserializeResult a = Either DeserializeError a

instance MonadFail (Either DeserializeError) where
  fail s = Left DeserializeFailed

class Deserialize a where
  deserialize :: String -> DeserializeResult a

newtype ObjectPath = MkObjectPath FilePath deriving (Show, Eq)

instance Serialize ObjectPath where
  serialize (MkObjectPath o) = o

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
  deserialize s =
    let parsed = head (parseMetaInfoSource s)
     in case parsed of
          [_, path, t, _] -> case parseDateTime8601 t of
            Just utc -> Right (MkMetaInfo (MkObjectPath path) (MkTrashedAt (utcTimeToTimestamp utc)))
            Nothing -> Left DeserializeFailed
          _ -> Left DeserializeFailed

parseMetaInfoSource :: String -> [[String]]
parseMetaInfoSource s = s =~ [reMI|path=(.+)\ntrashed-at=([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?Z).*|]

newtype TrashBox = MkTrashBox FilePath deriving (Show)
