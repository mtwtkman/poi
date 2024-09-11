module Poi.Test.Fixture.SrcData (create) where

import Poi.Test.Fixture (PoiTestData (..))

import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

create :: [PoiTestData] -> IO [FilePath]
create = mapM makeFile
 where
  makeFile :: PoiTestData -> IO FilePath
  makeFile (PoiTestData{poiTestDataParentNode = n, poiTestDataLeaf = l, isPoiTestDataDirectory = isDir}) = do
    let dest = joinPath [n, l]
    createDirectoryIfMissing True n
    if isDir then createDirectoryIfMissing True dest else writeFile dest "poi test"
    return dest
