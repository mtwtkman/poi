module Poi.Time (getCurrent) where
import Data.Time (getCurrentTimeZone, getCurrentTime, LocalTime, utcToLocalTime)

getCurrent :: IO LocalTime
getCurrent = do
  tz <- getCurrentTimeZone
  utcToLocalTime tz <$> getCurrentTime
