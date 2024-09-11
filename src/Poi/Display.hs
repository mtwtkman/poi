module Poi.Display (
  formatTrashCan,
  makeFullPath,
) where

import Data.Time (defaultTimeLocale, formatTime)
import Poi.Entity (
  Trash (
    Trash,
    trashOriginalParentPath,
    trashOriginalPath,
    trashedAt
  ),
 )
import System.FilePath (joinPath)

formatTrash :: Trash -> String
formatTrash (Trash{trashOriginalPath = o, trashOriginalParentPath = p, trashedAt = t}) = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S.%q" t <> "\t" <> joinPath [p, o]

formatTrashCan :: [Trash] -> [String]
formatTrashCan items =
  [ show index <> ": " <> formatTrash item
  | (index, item) <- zip ([1 ..] :: [Int]) items
  ]

makeFullPath :: Trash -> FilePath
makeFullPath (Trash{trashOriginalParentPath = p, trashOriginalPath = o}) = joinPath [p, o]
