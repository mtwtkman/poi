{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_, when)
import Data.List (intercalate)
import GHC.IO.Exception (ExitCode (ExitFailure))
import Poi.Abnormal (PoiAbnormal)
import Poi.Action (PoiAction (..))
import Poi.Action.Bury (
  deleteTrashByIndices,
  deleteTrashesByDayBefore,
  emptyTrashCan,
 )
import Poi.Action.ListUp (listUp)
import Poi.Action.PickUp (pickUpByIndices)
import Poi.Action.Toss (toss)
import Poi.Action.Type.Result (
  PoiActionError (CommonError, PoiBuryError, PoiTossError),
  PoiBuryError (BeforeDayMustBeZeroOrPositive, FilePathNotExist),
  PoiCommonError (
    FileNotFound,
    IndexMustBePositive,
    IndexOverFlow,
    TrashCanNotFound
  ),
 )
import Poi.Action.Version (showCurrentVersion)
import Poi.Cli (execPoiParser)
import Poi.Display (formatTrashCan, makeFullPath)
import Poi.Entity (
  OrderedTrashCan (OrderedTrashCan),
  SortOrder (Desc),
  TrashCanLocation (TrashCanLocation),
 )
import Poi.File.IO (
  FileIOError (FilePathNotFound),
  createTrashCanDirectory,
  findTrashCanLocation,
 )
import Poi.Prompt (PoiPromptError (InvalidInput), YN (No, Yes), confirm)
import Poi.Time (getCurrent)
import System.Exit (exitSuccess, exitWith)
import qualified Poi.TUI.App as TUI

doEmptyTrashCan :: TrashCanLocation -> IO ()
doEmptyTrashCan can = do
  yOrN <- confirm "Do you want to delete trashed files permanently?" '>'
  case yOrN of
    Right Yes -> do
      n <- emptyTrashCan can
      putStrLn
        ( "Empty trash can: "
            <> "deleted "
            <> show n
            <> " file"
            <> (if n > 1 then "s" else "")
            <> "."
        )
    Right No -> putStrLn "Do nothing."
    Left InvalidInput -> do
      putStrLn "Invalid value. Please input `Y(es)` or `N(o)`"
      doEmptyTrashCan can

actionErrorMsg :: PoiCommonError -> String
actionErrorMsg FileNotFound = "File not found."
actionErrorMsg (TrashCanNotFound l) = "Please ensure trash can location. TRASH_CAN_LOCATION_PATH=" <> show l
actionErrorMsg IndexMustBePositive = "Index must be positive."
actionErrorMsg IndexOverFlow = "Index too large."

abort :: (Show a, PoiAbnormal a) => a -> IO ()
abort e = print e >> exitWith (ExitFailure 1)

askPreparingTrashCanPath :: TrashCanLocation -> IO Bool
askPreparingTrashCanPath l = do
  ans <- confirm ("May I create " <> show l <> "?") '>'
  case ans of
    Left e -> abort e >> return False
    Right No -> putStrLn ("Please create " <> show l <> " manually") >> return False
    Right Yes -> createTrashCanDirectory l >> print ("created `" <> show l <> "`") >> return True

buryErrorMsg :: PoiBuryError -> String
buryErrorMsg FilePathNotExist = "File path not exist."
buryErrorMsg BeforeDayMustBeZeroOrPositive = "Before day option must be zero or positive number."

showErrorMsg :: PoiActionError -> IO ()
showErrorMsg e = do
  case e of
    CommonError e' -> putStrLn $ actionErrorMsg e'
    PoiBuryError e' -> putStrLn $ buryErrorMsg e'
    PoiTossError e' -> print e'
  exitWith (ExitFailure 1)

isPathFreeCommand :: PoiAction -> Bool
isPathFreeCommand ShowVersion = True
isPathFreeCommand _ = False

main :: IO ()
main = do
  action <- execPoiParser
  when (isPathFreeCommand action) (perform' action >> exitSuccess)

  pre <- findTrashCanLocation
  case pre of
    Right can -> perform action can
    Left (FilePathNotFound d) -> do
      let can = TrashCanLocation d
      proceed <- askPreparingTrashCanPath can
      when proceed $ perform action can

perform' :: PoiAction -> IO ()
perform' ShowVersion = showCurrentVersion >>= print
perform' _ = return ()

perform :: PoiAction -> TrashCanLocation -> IO ()
perform action can =
  case action of
    ListUp -> do
      result <- listUp Desc can
      case result of
        Right (OrderedTrashCan items _) -> do
          let (TrashCanLocation canS) = can
          putStrLn $ "=== POI Trash Can: " <> canS <> " ==="
          if null items
            then
              putStrLn "Empty."
            else
              putStr $ unlines $ formatTrashCan items
        Left e -> abort e
    Toss ps -> do
      tossed <- toss can ps
      case tossed of
        Right ts -> forM_ ts (putStrLn . ("tossed " <>) . makeFullPath)
        Left e -> abort e
    PickUpByIndex is -> do
      result <- pickUpByIndices can (map (+ negate 1) is)
      case result of
        Right picked -> putStrLn $ "picked up: " <> intercalate "," picked
        Left e -> abort e
    EmptyTrashCan -> doEmptyTrashCan can
    DeleteDayBefore d -> do
      t <- getCurrent
      result <- deleteTrashesByDayBefore can t d
      case result of
        Right ts -> putStrLn $ "Deleted" <> show (length ts) <> "files permanently."
        Left e -> abort e
    DeleteByIndex is -> do
      result <- deleteTrashByIndices can is
      case result of
        Right t -> putStrLn ("Deleted " <> intercalate "," (map makeFullPath t))
        Left e -> abort e
    StartTuiApplication -> TUI.start
    ShowVersion -> showCurrentVersion >>= print
