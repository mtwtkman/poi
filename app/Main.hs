{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.List (intercalate)
import GHC.IO.Exception (ExitCode (ExitFailure))
import Poi.Action (
  PoiAction (..),
  PoiActionError (CommonError, FileIOError, PoiBuryError),
  PoiBuryError (BeforeDayMustBeZeroOrPositive, FilePathNotExist),
  PoiCommonError (FileNotFound, IndexMustBePositive, IndexOverFlow, TrashCanNotFound),
  deleteTrashByIndices,
  deleteTrashesByDayBefore,
  emptyTrashCan,
  listUp,
  pickUpByIndices,
  toss,
 )
import Poi.Cli (execPoiParser)
import Poi.Display (formatTrashCan, makeFullPath)
import Poi.Entity (
  OrderedTrashCan (OrderedTrashCan),
  SortOrder (Desc),
  TrashCanLocation (TrashCanLocation),
 )
import Poi.File.IO (findTrashCanLocation)
import Poi.Prompt (PoiPromptError (InvalidInput), YN (No, Yes), confirm)
import Poi.Time (getCurrent)
import System.Exit (exitWith)

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

commonErrorMsg :: PoiCommonError -> String
commonErrorMsg FileNotFound = "File not found."
commonErrorMsg TrashCanNotFound = "Please ensure trash can location."
commonErrorMsg IndexMustBePositive = "Index must be positive."
commonErrorMsg IndexOverFlow = "Index too large."

buryErrorMsg :: PoiBuryError -> String
buryErrorMsg FilePathNotExist = "File path not exist."
buryErrorMsg BeforeDayMustBeZeroOrPositive = "Before day option must be zero or positive number."

showErrorMsg :: PoiActionError -> IO ()
showErrorMsg e = do
  case e of
    CommonError e' -> putStrLn $ commonErrorMsg e'
    PoiBuryError e' -> putStrLn $ buryErrorMsg e'
    FileIOError e' -> print e'
  exitWith (ExitFailure 1)

main :: IO ()
main = do
  action <- execPoiParser
  can <- findTrashCanLocation
  case action of
    ListUp -> do
      result <- listUp Desc can
      case result of
        Right (OrderedTrashCan items) -> do
          let (TrashCanLocation canS) = can
          putStrLn $ "=== POI Trash Can: " <> canS <> " ==="
          if null items
            then
              putStrLn "Empty."
            else
              putStr $ unlines $ formatTrashCan items
        Left e -> showErrorMsg e
    Toss ps -> do
      tossed <- toss can ps
      case tossed of
        Right ts -> forM_ ts (putStrLn . ("tossed " <>) . makeFullPath)
        Left e -> print e
    PickUpByIndex is -> do
      result <- pickUpByIndices can (map (+ negate 1) is)
      case result of
        Right picked -> putStrLn $ "picked up: " <> intercalate "," picked
        Left e -> showErrorMsg e
    EmptyTrashCan -> doEmptyTrashCan can
    DeleteDayBefore d -> do
      t <- getCurrent
      result <- deleteTrashesByDayBefore can t d
      case result of
        Right ts -> putStrLn $ "Deleted" <> show (length ts) <> "files permanently."
        Left e -> showErrorMsg e
    DeleteByIndex is -> do
      result <- deleteTrashByIndices can is
      case result of
        Right t -> putStrLn ("Deleted " <> intercalate "," (map makeFullPath t))
        Left e -> showErrorMsg e
