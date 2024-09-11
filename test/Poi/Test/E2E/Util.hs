{-# LANGUAGE TupleSections #-}

module Poi.Test.E2E.Util (
  withTestContext,
  PoiTestData (..),
  TestSrcRoot,
  TestCan,
  PackedTrashData,
  SignedTrashData,
  generateSequencialPackedData,
  packedDataToTrash,
  packPoiTestData,
  signData,
) where

import Control.Monad (forM_)
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.Time (
  LocalTime,
  NominalDiffTime,
  addLocalTime,
 )
import qualified Data.UUID.V4 as U
import Poi.Entity (Trash (Trash))
import Poi.Test.Fixture (
  PackedTrashData,
  PoiTestData (..),
  SignedTrashData,
 )
import System.Directory (createDirectoryIfMissing)
import System.FilePath (joinPath)
import System.IO.Temp (withSystemTempDirectory)
import Test.HUnit (Assertion)

type TestSrcRoot = FilePath
type TestCan = FilePath

withTestContext :: FilePath -> (TestSrcRoot -> TestCan -> IO ([PoiTestData], [Trash])) -> ((TestSrcRoot, [PoiTestData]) -> (TestCan, [Trash]) -> Assertion) -> IO ()
withTestContext can setup testBody = do
  withSystemTempDirectory
    "poi-test"
    ( \tmp -> do
        let testCan = joinPath [tmp, can]
            testSrcRoot = joinPath [tmp, "src"]
        forM_ [testCan, testSrcRoot] (createDirectoryIfMissing True)
        (td, ptd) <- setup testSrcRoot testCan
        testBody (testSrcRoot, td) (testCan, ptd)
    )

packPoiTestData :: LocalTime -> [PoiTestData] -> IO PackedTrashData
packPoiTestData t ds = do
  signed <- mapM (\d -> U.nextRandom <&> (,d)) ds
  return (t, signed)

generateSequencialPackedData :: NominalDiffTime -> LocalTime -> [[SignedTrashData]] -> IO [PackedTrashData]
generateSequencialPackedData duration start seeds = do
  result <-
    foldrM
      ( \seed (t, acc) -> do
          let dt = addLocalTime duration t
          return (dt, (dt, seed) : acc)
      )
      (start, [])
      seeds
  return $ snd result

packedDataToTrash :: LocalTime -> SignedTrashData -> Trash
packedDataToTrash t (fid, PoiTestData{poiTestDataLeaf = l, poiTestDataParentNode = n}) = Trash l n fid t

signData :: PoiTestData -> IO SignedTrashData
signData d = do
  fid <- U.nextRandom
  return (fid, d)
