{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Poi.Action (
  PoiAction (..),
  deleteTrashesByDayBefore,
  deleteTrashByIndex,
  emptyTrashCan,
  listUp,
  pickUpByIndex,
  toss,
 )
import Poi.Cli (execPoiParser)
import Poi.Display (formatTrashCan, makeFullPath)
import Poi.Entity (
  OrderedTrashCan (OrderedTrashCan),
  TrashCanLocation (TrashCanLocation),
 )
import Poi.File.IO (findTrashCanLocation)
import Poi.Prompt (PoiPromptError (InvalidInput), YN (No, Yes), confirm)
import Poi.Time (getCurrent)

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

main :: IO ()
main = do
  action <- execPoiParser
  can <- findTrashCanLocation
  case action of
    ListUp -> do
      OrderedTrashCan items <- listUp can
      let (TrashCanLocation canS) = can
      putStrLn $ "=== POI Trash Can: " <> canS <> " ==="
      if null items
        then
          putStrLn "Empty."
        else
          putStr $ unlines $ formatTrashCan items
    Toss p -> do
      tossed <- toss can [p]
      forM_ tossed (putStrLn . ("tossed " <>) . makeFullPath)
    PickUpByIndex i -> do
      result <- pickUpByIndex can (i - 1)
      case result of
        Right picked -> putStrLn $ "picked up: " <> picked
        Left e -> putStrLn $ "failed by " <> show e
    EmptyTrashCan -> doEmptyTrashCan can
    DeleteDayBefore d -> do
      t <- getCurrent
      result <- deleteTrashesByDayBefore can t d
      case result of
        Right ts -> putStrLn $ "Deleted" <> show (length ts) <> "files permanently."
        Left e -> print e
    DeleteByIndex i -> do
      result <- deleteTrashByIndex can i
      case result of
        Right t -> putStrLn ("Deleted " <> makeFullPath t)
        Left e -> print e
