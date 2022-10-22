{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Poi.Entity where

import Data.List
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Poi.Time
import System.FilePath
import Text.RE.Replace
import Text.RE.TDFA.String

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

trashedAtNow :: IO TrashedAt
trashedAtNow = do
  t <- getCurrentTime
  return . MkTrashedAt $ utcTimeToTimestamp t

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
  serialize a = intercalate "," $ map (\(k, v) -> k ++ "=" ++ v) [("path", serialize $ getObjectPath a), ("trashed-at", serialize $ getTrashedAt a)]

instance Deserialize MetaInfo where
  deserialize s =
    let captured = parseMetaInfoSource s
        p = capturedPath captured
        t = capturedTrashedAt captured
     in case (p, t) of
          (Just p', Just t') -> case parseDateTime8601 t' of
            Just utc -> Right (MkMetaInfo (MkObjectPath p') (MkTrashedAt (utcTimeToTimestamp utc)))
            Nothing -> Left DeserializeFailed
          _ -> Left DeserializeFailed

capturePath :: CaptureID
capturePath = IsCaptureName . CaptureName . pack $ "path"

captureTrashedAt :: CaptureID
captureTrashedAt = IsCaptureName . CaptureName . pack $ "trashedAt"

data CapturedMetaInfo = MkCapturedMetaInfo
  { capturedPath :: Maybe String,
    capturedTrashedAt :: Maybe String
  } deriving (Show, Eq)

parseMetaInfoSource :: String -> CapturedMetaInfo
parseMetaInfoSource s =
  let matched = s ?=~ [reBI|path=${path}(.+),trashed-at=${trashedAt}([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?Z).*|]
   in MkCapturedMetaInfo (matched !$$? capturePath) (matched !$$? captureTrashedAt)

newtype TrashBox = MkTrashBox FilePath deriving (Show)

metaInfoFileLocation :: TrashBox -> FilePath
metaInfoFileLocation (MkTrashBox path) = path </> "metainfo"

storeDirPath :: TrashBox -> MetaInfo -> FilePath
storeDirPath (MkTrashBox d) m = d </> serialize (getTrashedAt m)
