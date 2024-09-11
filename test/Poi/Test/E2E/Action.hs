{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Poi.Test.E2E.Action where

import Control.Monad (forM, forM_)
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.List (sortBy)
import qualified Data.Set as S
import Data.Time (
  LocalTime (LocalTime),
  TimeOfDay (TimeOfDay),
  addLocalTime,
  fromGregorian,
  nominalDay,
  secondsToNominalDiffTime,
 )
import qualified Data.UUID as U
import Poi.Action (
  IndexSpecified,
  PoiActionError (..),
  PoiBuryError (..),
  PoiCommonError (..),
  deleteTrashByIndex,
  deleteTrashesByDayBefore,
  duplicationSafeName,
  emptyTrashCan,
  pickUpByIndex,
 )
import Poi.Entity (
  OrderedTrashCan (OrderedTrashCan),
  SortOrder (Desc),
  Trash (Trash, trashId, trashOriginalPath, trashedAt),
  TrashCan (TrashCan),
  TrashCanLocation (TrashCanLocation),
  buildTAbsoluteTrashFilePath,
  buildTrashedAtPath,
  formatTrashedAt,
  sortTrashes,
 )
import Poi.Test.E2E.Util (
  PoiTestData (PoiTestData, poiTestDataLeaf, poiTestDataParentNode),
  packPoiTestData,
  withTestContext,
 )
import Poi.Test.Fixture (
  asciiStringNamedDirectory,
  goldenData,
  makeFullPathFromPoiTestData,
  singleAsciiStringNamedFile,
 )
import qualified Poi.Test.Fixture.SrcData as SD
import qualified Poi.Test.Fixture.TrashedData as TD
import Poi.Test.Util (pickRandom)
import Poi.Time (getCurrent)
import System.Directory (doesPathExist)
import System.FilePath (joinPath)
import Test.Tasty (TestTree)
import Test.Tasty.HUnit (testCase, (@?), (@?=))
import Test.Tasty.Runners (TestTree (TestGroup))

tests :: TestTree
tests =
  TestGroup
    "Poi.Entity.Action e2e test"
    [ test_pickUpByIndex
    , test_emptyTrashCan
    , test_deleteTrashByIndex
    , test_deleteTrashesByDayBefore
    ]

indexViolationTestCase :: (Show a, Eq a) => String -> IndexSpecified a -> [TestTree]
indexViolationTestCase entrypointName testFunc =
  map
    ( \(i, label, expected) ->
        testCase ("should be error against invalid index" <> "[" <> label <> "]") $
          withTestContext
            entrypointName
            (\_ _ -> return ([], []))
            ( \_ (testCan, _) -> do
                actual <- testFunc (TrashCanLocation testCan) i
                actual @?= Left (CommonError expected)
            )
    )
    [(0, "overflow", IndexOverFlow), (-1, "negative", IndexMustBePositive)]

test_pickUpByIndex :: TestTree
test_pickUpByIndex =
  TestGroup
    "pickUpByIndex"
    ( [ testCase "should be able to pick up trashed one from trash can" $ do
          current <- getCurrent
          withTestContext
            "pick-up-by-index-test"
            ( \testSrcRoot testCan -> do
                let testData = goldenData testSrcRoot
                trashed <- packPoiTestData current testData >>= TD.create testCan . (: [])
                return (testData, trashed)
            )
            ( \(_, testData) (testCan, _) -> do
                forM_
                  (sortBy (flip compare) testData)
                  ( \(PoiTestData{poiTestDataParentNode = en, poiTestDataLeaf = el}) -> do
                      let can = TrashCanLocation testCan
                          expected = joinPath [en, el]
                      actual <- pickUpByIndex can 0
                      actual @?= Right expected
                      doesPathExist expected >>= (@?= True)
                  )
            )
      , testCase "should avoid to overwrite on duplicated filepath" $ do
          current <- getCurrent
          withTestContext
            "pick-up-by-index-dup-test"
            ( \testSrcRoot testCan -> do
                let testData = [singleAsciiStringNamedFile testSrcRoot]
                trashed <- packPoiTestData current testData >>= TD.create testCan . (: [])
                _ <- SD.create testData
                return (testData, trashed)
            )
            ( \(_, testData) (testCan, trashed) -> do
                let can = TrashCanLocation testCan
                    (Trash{trashId = fid}) = head trashed
                    expected = makeFullPathFromPoiTestData (head testData) <> duplicationSafeName (U.toString fid)
                actual <- pickUpByIndex can 0
                actual @?= Right expected
            )
      ]
        <> indexViolationTestCase "pick-up-by-index-violation" pickUpByIndex
    )

test_emptyTrashCan :: TestTree
test_emptyTrashCan =
  TestGroup
    "emptyTrashCan"
    ( map
        ( \(d, label) -> testCase ("should be able to delete file permanentry [" <> label <> "]") $ do
            current <- getCurrent
            withTestContext
              "empty-trash-can"
              ( \testSrcRoot testCan -> do
                  let testData = [d testSrcRoot]
                  trashed <- packPoiTestData current testData >>= TD.create testCan . (: [])
                  return (testData, trashed)
              )
              ( \(_, testData) (testCan, trashed) -> do
                  let can = TrashCanLocation testCan
                  actual <- emptyTrashCan can
                  actual @?= length testData
                  forM_
                    trashed
                    ( \(Trash{trashId = fid, trashedAt = t}) -> do
                        let expected = joinPath [testCan, formatTrashedAt t, U.toString fid]
                        doesPathExist expected <&> (@? "Filemust be delete actually") . not
                    )
              )
        )
        [(singleAsciiStringNamedFile, "file"), (asciiStringNamedDirectory, "directory")]
        <> [ testCase "should do nothing against already empty trash can" $
              withTestContext
                "empty-trash-can-do-nothing"
                (\_ _ -> return ([], []))
                ( \_ (testCan, _) -> do
                    actual <- emptyTrashCan (TrashCanLocation testCan)
                    actual @?= 0
                )
           ]
    )

assertTrashedFileRemaining :: TrashCanLocation -> [Trash] -> IO Bool
assertTrashedFileRemaining can = foldrM (\t r -> if r then buildTAbsoluteTrashFilePath can t >>= doesPathExist else return False) True

test_deleteTrashByIndex :: TestTree
test_deleteTrashByIndex =
  TestGroup
    "deleteTrashByIndex"
    ( [ testCase "should be able to delete specified one correctly" $ do
          current <- getCurrent
          withTestContext
            "delete-trash-by-index"
            ( \testSrcRoot testCan -> do
                let testData = goldenData testSrcRoot
                trashed <- packPoiTestData current testData >>= TD.create testCan . (: [])
                return (testData, trashed)
            )
            ( \_ (testCan, trashed) -> do
                let (OrderedTrashCan sorted) = sortTrashes Desc (TrashCan $ S.fromList trashed)
                (i, expected) <- pickRandom sorted
                let expectedRemaining = takeWhile (/= expected) trashed <> tail (dropWhile (/= expected) trashed)
                    can = TrashCanLocation testCan
                actual <- deleteTrashByIndex can i
                actual @?= Right expected
                assertTrashedFileRemaining can expectedRemaining >>= (@?= True)
            )
      , testCase "should be error against the file not exist" $
          withTestContext
            "delete-trash-by-index-unknown"
            (\_ _ -> return ([], []))
            ( \_ (testCan, _) -> do
                actual <- deleteTrashByIndex (TrashCanLocation testCan) 1
                actual @?= Left (CommonError IndexOverFlow)
            )
      , testCase "should delete empty trashedAt Path" $ do
          current <- getCurrent
          withTestContext
            "delete-trah-by-index-empty"
            ( \testSrc testCan -> do
                trashed <- packPoiTestData current [singleAsciiStringNamedFile testSrc] >>= TD.create testCan . (: [])
                return ([], trashed)
            )
            ( \_ (testCan, _) -> do
                let can = TrashCanLocation testCan
                _ <- deleteTrashByIndex can 0
                doesPathExist (buildTrashedAtPath can current) >>= (@?= False)
            )
      ]
        <> indexViolationTestCase "delete-trash-by-index-violation" deleteTrashByIndex
    )

test_deleteTrashesByDayBefore :: TestTree
test_deleteTrashesByDayBefore =
  TestGroup
    "deleteTrashesByDayBefore"
    [ testCase "should be able to delete files which were trashed specified days before" $ do
        let current = LocalTime (fromGregorian 2024 10 1) (TimeOfDay 9 0 0)
            before1Sec = addLocalTime (negate (secondsToNominalDiffTime 1)) current
            before1Day = addLocalTime (negate nominalDay) before1Sec
            before2Days = addLocalTime (negate nominalDay) before1Day
            targets =
              [ ("target-1", before1Day)
              , ("target-2", before2Days)
              ]
            nonTargets =
              [ ("non-target-1", before1Sec)
              , ("non-target-2", current)
              ]
        withTestContext
          "delete-trash-by-day-before"
          ( \testSrcRoot testCan -> do
              let
                testData =
                  map
                    (\(fname, t) -> (t, [PoiTestData (joinPath [testSrcRoot, "parent"]) fname False]))
                    (targets <> nonTargets)
              packed <- forM testData (uncurry packPoiTestData)
              trashed <- TD.create testCan packed
              return (foldr (\(_, x) acc -> x <> acc) [] testData, trashed)
          )
          ( \(_, _testData) (testCan, trashed) -> do
              let can = TrashCanLocation testCan
                  expected = filter (\t -> trashOriginalPath t `elem` map fst targets) trashed
                  expectedRemaining = filter (\t -> trashOriginalPath t `elem` map fst nonTargets) trashed
              actual <- deleteTrashesByDayBefore can before1Sec 1
              actual @?= Right (S.fromList expected)
              assertTrashedFileRemaining can expectedRemaining >>= (@?= True)
          )
    , testCase "should do nothing against no matched file" $ do
        current <- getCurrent
        withTestContext
          "delete-trash-by-day-before-do-nothing"
          ( \testSrcRoot testCan -> do
              let testData = goldenData testSrcRoot
              trashed <- packPoiTestData current testData >>= TD.create testCan . (: [])
              return (testData, trashed)
          )
          ( \_ (testCan, _) -> do
              actual <- deleteTrashesByDayBefore (TrashCanLocation testCan) current 9999
              actual @?= Right S.empty
          )
    , testCase "should be error against negative value" $ do
        current <- getCurrent
        withTestContext
          "delete-trash-by-day-before-abnormal"
          ( \testSrcRoot testCan -> do
              packed <- packPoiTestData current [singleAsciiStringNamedFile testSrcRoot]
              trashed <- TD.create testCan [packed]
              return ([], trashed)
          )
          ( \_ (testCan, trashed) -> do
              actual <- deleteTrashesByDayBefore (TrashCanLocation testCan) current (-1)
              actual @?= Left (PoiBuryError BeforeDayMustBeZeroOrPositive)
              forM_
                trashed
                (\t -> buildTAbsoluteTrashFilePath (TrashCanLocation testCan) t >>= doesPathExist >>= (@? "File must be existed"))
          )
    ]
