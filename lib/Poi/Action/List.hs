module Poi.Action.List where

import Poi.Entity

indexedList :: [a] -> [(Int, a)]
indexedList = go [] 1
  where
    go acc index (x : xs) = go (acc ++ [(index, x)]) (index + 1) xs
    go acc index [] = acc

listIndexedAll :: [MetaInfo] -> [(Int, MetaInfo)]
listIndexedAll = listIndexedAll
