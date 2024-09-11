module Poi.Test.Fixture.TrashedData (create) where

import Poi.Test.Fixture (
  PackedTrashData,
  PoiTestData (..),
  SignedTrashData,
 )

import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.Time (LocalTime)
import qualified Data.UUID as U
import Poi.Entity (
  Trash (Trash),
  formatTrashedAt,
  trashContainerName,
 )
import Poi.File.IO (saveParentLocation)
import Poi.Test.E2E.Util (TestCan)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)

create :: TestCan -> [PackedTrashData] -> IO [Trash]
create testCan = foldrM (\(t, signed) acc -> mapM (makeFile t) signed <&> (<> acc)) []
 where
  makeFile :: LocalTime -> SignedTrashData -> IO Trash
  makeFile t (fid, PoiTestData{poiTestDataParentNode = n, poiTestDataLeaf = l, isPoiTestDataDirectory = isDir}) = do
    let trashedAtPath = joinPath [testCan, formatTrashedAt t]
        container = joinPath [trashedAtPath, U.toString fid]
        trashContainer = joinPath [container, trashContainerName]
        dest = joinPath [trashContainer, l]
    createDirectoryIfMissing True trashContainer
    saveParentLocation container n
    if isDir then createDirectoryIfMissing True dest else writeFile dest "poi test"
    return $ Trash l n fid t
