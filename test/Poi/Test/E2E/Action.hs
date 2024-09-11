{-# LANGUAGE RankNTypes #-}

module Poi.Test.E2E.Action where

import Control.Monad (forM_)
import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.List (sort, sortBy, (\\))
import qualified Data.UUID as U
import Poi.Action (
  IndexSpecified,
  PoiActionError (..),
  PoiBuryError (..),
  PoiCommonError (..),
  deleteTrashByIndex,
  deleteTrashesByDayBefore,
  emptyTrashCan,
  pickUpByIndex,
 )
import Poi.Display (makeFullPath)
import Poi.Entity (
  Trash (Trash, trashId, trashedAt),
  TrashCanLocation (TrashCanLocation),
  formatTrashedAt,
 )
import Poi.Test.E2E.Util (
  PoiTestData (PoiTestData, poiTestDataLeaf, poiTestDataParentNode),
  packPoiTestData,
  withTestContext,
 )
import Poi.Test.Fixture (
  asciiStringNamedDirectory,
  goldenData,
  singleAsciiStringNamedFile,
 )
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
                  (zip [0 ..] (sortBy (flip compare) testData))
                  ( \(i, PoiTestData{poiTestDataParentNode = en, poiTestDataLeaf = el}) -> do
                      let can = TrashCanLocation testCan
                          expected = joinPath [en, el]
                      pickUpByIndex can i >> doesPathExist expected >>= (@?= True)
                  )
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
assertTrashedFileRemaining (TrashCanLocation can) = foldrM (\t r -> if not r then return r else doesPathExist (joinPath [can, makeFullPath t])) False

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
                (i, expected) <- pickRandom trashed
                let expectedRemaining = takeWhile (/= expected) trashed <> tail (dropWhile (/= expected) trashed)
                let can = TrashCanLocation testCan
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
                actual @?= Left (PoiBuryError FilePathNotExist)
            )
      ]
        <> indexViolationTestCase "delete-trash-by-index-violation" deleteTrashByIndex
    )

test_deleteTrashesByDayBefore :: TestTree
test_deleteTrashesByDayBefore =
  TestGroup
    "deleteTrashesByDayBefore"
    [ testCase "should be able to delete files which were trashed specified days before" $ do
        current <- getCurrent
        withTestContext
          "delete-trash-by-day-before"
          ( \testSrcRoot testCan -> do
              let testData = goldenData testSrcRoot
              trashed <- packPoiTestData current testData >>= TD.create testCan . (: [])
              return (testData, trashed)
          )
          ( \(_, testData) (testCan, trashed) -> do
              let can = TrashCanLocation testCan
                  half = length testData `div` 2
                  ordered = sort trashed
                  expected = take half ordered
                  expectedRemaining = ordered \\ expected
              actual <- deleteTrashesByDayBefore can current half
              actual @?= Right expected
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
              actual @?= Right []
          )
    , testCase "should be error against negative value" $ do
        current <- getCurrent
        withTestContext
          "delete-trash-by-day-before-abnormal"
          (\_ _ -> return ([], []))
          ( \_ (testCan, _) -> do
              actual <- deleteTrashesByDayBefore (TrashCanLocation testCan) current (-1)
              actual @?= Left (PoiBuryError BeforeDayMustBeZeroOrPositive)
          )
    ]
