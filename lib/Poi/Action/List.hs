module Poi.Action.List where

import Data.List
import Data.Time.Format.ISO8601
import Data.Time.LocalTime
import Poi.Action.UpdateMetaInfo
import Poi.Entity
import Poi.Time

indexedList :: Int -> [a] -> [(Int, a)]
indexedList = go []
  where
    go acc index (x : xs) = go (acc ++ [(index, x)]) (index + 1) xs
    go acc index [] = acc

listIndexedAll :: [MetaInfo] -> [(Int, MetaInfo)]
listIndexedAll = listIndexedAll

showIndexedMetaInfo :: TimeZone -> (Int, MetaInfo) -> String
showIndexedMetaInfo tz (index, m) = show index ++ ": " ++ localTrashedAt ++ " " ++ serialize (unObjectPath m)
  where
    (MkTrashedAt t) = unTrashedAt m
    localTrashedAt = iso8601Show $ timestampToLocalTime tz t

showIndexedMetaInfoList :: TimeZone -> [MetaInfo] -> String
showIndexedMetaInfoList tz ms = intercalate "\n" $ map (showIndexedMetaInfo tz) (listIndexedAll ms)

printIndexedMetaInfoList :: [MetaInfo] -> IO ()
printIndexedMetaInfoList ms = do
  tz <- getCurrentTimeZone
  putStrLn $ showIndexedMetaInfoList tz ms

printCurrentIndexedMetaInfoList :: TrashBox -> IO ()
printCurrentIndexedMetaInfoList tb = do
  m <- getCurrentWholeMetaInfo tb
  case m of
    Right ms -> printIndexedMetaInfoList ms
    Left reason -> print $ show reason
