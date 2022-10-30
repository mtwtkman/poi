module Poi.Action.Prompt where

import Control.Monad
import Data.Either
import Poi.Action.List
import Poi.Action.UpdateMetaInfo
import Poi.Entity
import Text.Read

validIndex :: Int -> [MetaInfo] -> Bool
validIndex i ms = i >= 0 && length ms > i

askMetaInfoIndex :: TrashBox -> IO Int
askMetaInfoIndex tb = do
  printCurrentIndexedMetaInfoList tb
  msResult <- getCurrentWholeMetaInfo tb
  when (isLeft msResult) (fail "TrashBox is empty")
  let (Right ms) = msResult
  question tb ms
  where
    question tb ms = do
      putStr "Select number to manipulate >"
      input <- getLine
      case readMaybe input of
        Nothing -> do
          print "Please input the value from index number"
          question tb ms
        Just choosen -> do
          if validIndex choosen ms
            then return choosen
            else do
              print "Please input the value from index number"
              question tb ms
