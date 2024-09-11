module Poi.Test.Fixture (
  PoiTestData (..),
  PackedTrashData,
  SignedTrashData,
  singleAsciiStringNamedFile,
  nestedAsciiStringNamedFile,
  singleUnicodeStringNamedFile,
  nestedUnicodeStringNamedFile,
  asciiStringNamedDirectory,
  unicodeStringNamedDirectory,
  createPoiTestData,
  goldenData,
  makeFullPathFromPoiTestData,
) where

import Data.Time (LocalTime)
import qualified Data.UUID as U
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath, splitDrive)

type PackedTrashData = (LocalTime, [SignedTrashData])
type SignedTrashData = (U.UUID, PoiTestData)

data PoiTestData = PoiTestData
  { poiTestDataParentNode :: FilePath
  , poiTestDataLeaf :: FilePath
  , isPoiTestDataDirectory :: Bool
  }
  deriving (Show, Ord, Eq)

createPoiTestData :: FilePath -> PoiTestData -> IO FilePath
createPoiTestData root (PoiTestData{poiTestDataParentNode = n, poiTestDataLeaf = l, isPoiTestDataDirectory = isDir}) =
  let parentDir = joinPath [root, snd $ splitDrive n]
      dest = joinPath [parentDir, l]
   in ( if isDir
          then
            createDirectoryIfMissing True dest
          else
            createDirectoryIfMissing True parentDir >> writeFile dest "test"
      )
        >> return dest

makeFullPathFromPoiTestData :: PoiTestData -> FilePath
makeFullPathFromPoiTestData (PoiTestData{poiTestDataParentNode = n, poiTestDataLeaf = l}) = joinPath [n, l]

singleAsciiStringNamedFile :: FilePath -> PoiTestData
singleAsciiStringNamedFile root = PoiTestData (joinPath [root, "single-parent"]) "single.txt" False

nestedAsciiStringNamedFile :: FilePath -> PoiTestData
nestedAsciiStringNamedFile root = PoiTestData (joinPath [root, "this/is/a"]) "nested.txt" False

singleUnicodeStringNamedFile :: FilePath -> PoiTestData
singleUnicodeStringNamedFile root = PoiTestData (joinPath [root, "しんぐる/親階層"]) "しんぐる.txt" False

nestedUnicodeStringNamedFile :: FilePath -> PoiTestData
nestedUnicodeStringNamedFile root = PoiTestData (joinPath [root, "これ/は"]) "ねすてっど.txt" False

asciiStringNamedDirectory :: FilePath -> PoiTestData
asciiStringNamedDirectory root = PoiTestData (joinPath [root, "this/is/a"]) "directory" True

unicodeStringNamedDirectory :: FilePath -> PoiTestData
unicodeStringNamedDirectory root = PoiTestData (joinPath [root, "これ/は"]) "でぃれくとり" True

goldenData :: FilePath -> [PoiTestData]
goldenData root =
  map
    ($ root)
    [ singleAsciiStringNamedFile
    , singleUnicodeStringNamedFile
    , nestedAsciiStringNamedFile
    , nestedUnicodeStringNamedFile
    , asciiStringNamedDirectory
    , unicodeStringNamedDirectory
    ]
