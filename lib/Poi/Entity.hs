{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}

module Poi.Entity where

import Data.List
import Data.Text (pack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Data.UUID
import Data.UUID.V4
import Poi.Time
import System.FilePath
import Text.RE.Replace
import Text.RE.TDFA.String
import System.Directory

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

absolutePath :: FilePath -> IO FilePath
absolutePath p
  | isAbsolute p = return p
  | otherwise = do
    d <- getCurrentDirectory
    return $ d </> p

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
  { unObjectPath :: ObjectPath,
    unTrashedAt :: TrashedAt,
    unId :: UUID
  }
  deriving (Show, Eq)

instance Serialize MetaInfo where
  serialize a = intercalate "," $ map (\(k, v) -> k ++ "=" ++ v) [("id", toString $ unId a), ("path", serialize $ unObjectPath a), ("trashed-at", serialize $ unTrashedAt a)]

instance Deserialize MetaInfo where
  deserialize s =
    let captured = parseMetaInfoSource s
        i = capturedId captured
        p = capturedPath captured
        t = capturedTrashedAt captured
     in case (i, p, t) of
          (Just i', Just p', Just t') -> case (parseDateTime8601 t', fromString i') of
            (Just utc, Just objectId) -> Right (MkMetaInfo (MkObjectPath p') (MkTrashedAt (utcTimeToTimestamp utc)) objectId)
            _ -> Left DeserializeFailed
          _ -> Left DeserializeFailed

makeCaptureID :: String -> CaptureID
makeCaptureID = IsCaptureName . CaptureName . pack

captureId :: CaptureID
captureId = makeCaptureID "id"

capturePath :: CaptureID
capturePath = makeCaptureID "path"

captureTrashedAt :: CaptureID
captureTrashedAt = makeCaptureID "trashedAt"

data CapturedMetaInfo = MkCapturedMetaInfo
  { capturedPath :: Maybe String,
    capturedTrashedAt :: Maybe String,
    capturedId :: Maybe String
  }
  deriving (Show, Eq)

parseMetaInfoSource :: String -> CapturedMetaInfo
parseMetaInfoSource s =
  let matched = s ?=~ [reBI|id=${id}([0-9a-z]{8}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{4}-[0-9a-z]{12}),path=${path}(.+),trashed-at=${trashedAt}([0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\.[0-9]+)?Z).*|]
   in MkCapturedMetaInfo (matched !$$? capturePath) (matched !$$? captureTrashedAt) (matched !$$? captureId)

newtype TrashBox = MkTrashBox FilePath deriving (Show)

metaInfoFileLocation :: TrashBox -> FilePath
metaInfoFileLocation (MkTrashBox path) = path </> "metainfo"

metaInfoFromFilePath :: FilePath -> IO MetaInfo
metaInfoFromFilePath p = do
  uuid <- nextRandom
  t <- getCurrentTimestamp
  return $ MkMetaInfo (MkObjectPath p) (MkTrashedAt t) uuid
