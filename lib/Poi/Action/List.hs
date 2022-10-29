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
listIndexedAll = indexedList 0

metaInfoListRow :: String -> TimeZone -> MetaInfo -> String
metaInfoListRow prefix tz m = prefix ++ localTrashedAt ++ " " ++ serialize (unObjectPath m)
  where
    (MkTrashedAt t) = unTrashedAt m
    localTrashedAt = iso8601Show $ timestampToLocalTime tz t

indexedMetaInfoListRow :: Int -> TimeZone -> MetaInfo -> String
indexedMetaInfoListRow index = metaInfoListRow (show index ++ ": ")

buildMetaInfoList :: TimeZone -> [MetaInfo] -> String
buildMetaInfoList tz ms = unlines $ map (metaInfoListRow "" tz) ms

buildIndexedMetaInfoList :: TimeZone -> [MetaInfo] -> String
buildIndexedMetaInfoList tz ms = unlines $ map (\(i, m) -> indexedMetaInfoListRow i tz m) (listIndexedAll ms)

printIndexedMetaInfoList :: [MetaInfo] -> IO ()
printIndexedMetaInfoList ms = do
  tz <- getCurrentTimeZone
  putStrLn $ buildIndexedMetaInfoList tz ms

printMetaInfoList :: [MetaInfo] -> IO ()
printMetaInfoList ms = do
  tz <- getCurrentTimeZone
  putStrLn $ buildMetaInfoList tz ms

printCurrentIndexedMetaInfoList :: TrashBox -> IO ()
printCurrentIndexedMetaInfoList tb = do
  m <- getCurrentWholeMetaInfo tb
  case m of
    Right ms -> printIndexedMetaInfoList ms
    Left reason -> print $ show reason

printCurrentMetaInfoList :: TrashBox -> IO ()
printCurrentMetaInfoList tb = do
  m <- getCurrentWholeMetaInfo tb
  case m of
    Right ms -> printMetaInfoList ms
    Left reason -> print $ show reason
