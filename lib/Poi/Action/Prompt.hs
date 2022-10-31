module Poi.Action.Prompt where

import Control.Monad
import Data.Either
import Poi.Action.List
import Poi.Action.UpdateMetaInfo
import Poi.Entity
import System.IO
import Text.Read

validIndex :: Int -> [MetaInfo] -> Bool
validIndex i ms = i >= 0 && length ms > i

askMetaInfo :: TrashBox -> IO MetaInfo
askMetaInfo tb = do
  printCurrentIndexedMetaInfoList tb
  msResult <- getCurrentWholeMetaInfo tb
  when (isLeft msResult) (fail "TrashBox is empty")
  let (Right ms) = msResult
  index <- question tb ms
  return $ ms !! index
  where
    question tb ms = do
      putStr "Select number to manipulate > "
      hFlush stdout
      input <- getLine
      case readMaybe input of
        Nothing -> do
          putStrLn "Please input the value from index number"
          question tb ms
        Just choosen -> do
          if validIndex choosen ms
            then return choosen
            else do
              putStrLn "Please input the value from index number"
              question tb ms
