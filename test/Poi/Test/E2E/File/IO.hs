{-# HLINT ignore "Use camelCase" #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Poi.Test.E2E.File.IO (tests) where

import qualified Data.Set as S
import qualified Poi.Test.Fixture.SrcData as SD
import qualified Poi.Test.Fixture.TrashedData as TD
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@?=))
import Test.Tasty.Runners (TestTree (TestGroup))

import Data.Functor ((<&>))
import Data.Time (
  LocalTime,
  addLocalTime,
  nominalDay,
  secondsToNominalDiffTime,
 )
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U
import Poi.Entity (
  Trash (..),
  TrashCan (TrashCan),
  TrashCanLocation (TrashCanLocation),
  buildTrashedAtPath,
  parentFileName,
 )
import Poi.File.IO (digTrashCan, trashToCan)
import Poi.Test.E2E.Util (
  PoiTestData (..),
  SignedTrashData,
  TestCan,
  TestSrcRoot,
  withTestContext,
 )
import Poi.Test.Fixture (
  PackedTrashData,
  asciiStringNamedDirectory,
  goldenData,
  makeFullPathFromPoiTestData,
  nestedAsciiStringNamedFile,
  nestedUnicodeStringNamedFile,
  singleAsciiStringNamedFile,
  singleUnicodeStringNamedFile,
  unicodeStringNamedDirectory,
 )
import Poi.Time (getCurrent)
import System.FilePath (joinPath)

tests :: TestTree
tests =
  TestGroup
    "Poi.Entity e2e test"
    [ test_digTrashCan
    , test_trashToCan
    ]

generateTrashedData :: LocalTime -> TestSrcRoot -> IO [PackedTrashData]
generateTrashedData t testSrcRoot = do
  let a_second_after = addLocalTime (secondsToNominalDiffTime 1) t
      a_minuite_after = addLocalTime (secondsToNominalDiffTime 60) t
      an_hour_after = addLocalTime (secondsToNominalDiffTime (60 * 60)) t
      tomorrow = addLocalTime nominalDay t
  mapM
    (\(lt, fs) -> identify fs <&> (lt,))
    [ (t, [singleAsciiStringNamedFile])
    , (a_second_after, [singleAsciiStringNamedFile])
    , (a_minuite_after, [singleAsciiStringNamedFile])
    , (an_hour_after, [singleAsciiStringNamedFile])
    ,
      ( tomorrow
      ,
        [ singleUnicodeStringNamedFile
        , nestedAsciiStringNamedFile
        , nestedUnicodeStringNamedFile
        , asciiStringNamedDirectory
        , unicodeStringNamedDirectory
        ]
      )
    ]
 where
  identify :: [FilePath -> PoiTestData] -> IO [SignedTrashData]
  identify = mapM (\f -> U.nextRandom >>= \fid -> return (fid, f testSrcRoot))

test_digTrashCan :: TestTree
test_digTrashCan =
  TestGroup
    "digTrashCan"
    [ testCase "should make a Trash data from an actual trash can" $ do
        dataGenerator <- getCurrent <&> generateTrashedData
        withTestContext
          "dig-trash-can-test"
          ( \testSrcRoot testCan -> do
              trashed <- dataGenerator testSrcRoot >>= TD.create testCan
              return ([], trashed)
          )
          ( \_ (testCan, trashed) -> do
              let expected = TrashCan $ S.fromList trashed
              actual <- digTrashCan (TrashCanLocation testCan)
              actual @?= expected
          )
    ]

test_trashToCan :: TestTree
test_trashToCan =
  TestGroup
    "trashToCan"
    [ testCase "should toss passed data to trashcan" $
        runner
          ( \_ _ args actual -> do
              map (\(Trash{trashOriginalPath = op, trashOriginalParentPath = pp}) -> joinPath [pp, op]) actual @?= args
          )
    , testCase "should write parent location file" $
        runner
          ( \testSrcRoot testCan _ actual -> do
              let expected = map poiTestDataParentNode (goldenData testSrcRoot)
              actualParents <-
                mapM
                  ( \(Trash{trashedAt = t, trashId = fid}) -> do
                      readFile (joinPath [buildTrashedAtPath (TrashCanLocation testCan) t, U.toString fid, parentFileName])
                  )
                  actual
              actualParents @?= expected
          )
    ]
 where
  runner :: (TestSrcRoot -> TestCan -> [FilePath] -> [Trash] -> Assertion) -> IO ()
  runner testbody = do
    withTestContext
      "trash-to-can-test"
      ( \testSrcRoot _ -> do
          let testData = goldenData testSrcRoot
          _ <- SD.create testData
          return (testData, [])
      )
      ( \(testSrcRoot, testData) (testCan, _) -> do
          let src = map makeFullPathFromPoiTestData testData
          current <- getCurrent
          actual <- trashToCan (TrashCanLocation testCan) src current
          testbody testSrcRoot testCan src actual
      )
