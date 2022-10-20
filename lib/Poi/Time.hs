module Poi.Time
  ( epochtime,
    utcTimeToTimestamp,
    timestampToUTCTime,
    posixTimeToLocalTime,
    timestampToLocalTime,
    Timestamp (..),
    getCurrentTimestamp,
  )
where

import Data.Fixed
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.LocalTime

newtype Timestamp = MkTimestamp Pico deriving (Eq, Show)

epochtime :: UTCTime -> Pico
epochtime = nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds

utcTimeToTimestamp :: UTCTime -> Timestamp
utcTimeToTimestamp = MkTimestamp . epochtime

timestampToUTCTime :: Timestamp -> UTCTime
timestampToUTCTime (MkTimestamp t) = posixSecondsToUTCTime $ secondsToNominalDiffTime t

posixTimeToLocalTime :: TimeZone -> POSIXTime -> LocalTime
posixTimeToLocalTime tz = utcToLocalTime tz . posixSecondsToUTCTime

timestampToLocalTime :: TimeZone -> Timestamp -> LocalTime
timestampToLocalTime tz = posixTimeToLocalTime tz . utcTimeToPOSIXSeconds . timestampToUTCTime

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp = do utcTimeToTimestamp <$> getCurrentTime
