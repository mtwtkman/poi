module Main where

import Data.UUID as U
import Control.Monad (forM_, replicateM)
import Data.Functor ((<&>))
import Data.Time (LocalTime, addLocalTime, secondsToNominalDiffTime)
import Poi.File.IO (findTrashCanLocation, trashToCan)
import Poi.Time (getCurrent)
import System.Environment (lookupEnv)
import System.Random (randomRIO)
import Poi.Entity (TrashCanLocation)

defaultDataSize :: Int
defaultDataSize = 50

datasize :: IO Int
datasize = lookupEnv "DATASIZE" <&> maybe 50 read

randomLocalTime :: IO LocalTime
randomLocalTime = do
  let day = 24 * 60 * 60 :: Int
      lo = day * 180
  offset <- randomRIO (-lo, 0) <&> fromIntegral
  addLocalTime (secondsToNominalDiffTime offset) <$> getCurrent

randomFilePath :: IO FilePath
randomFilePath = do
  len <- randomRIO (1, 15)
  replicateM len randomPick
 where
  chars :: [Char]
  chars = ['あ' .. 'ん'] <> ['A' .. 'z']

  randomPick :: IO Char
  randomPick = randomRIO (0, length chars - 1) >>= \i -> return $ chars !! i

trash :: IO (LocalTime, FilePath)
trash = (,) <$> randomLocalTime <*> randomFilePath


buildTrashDest :: TrashCanLocation -> LocalTime -> IO FilePath
buildTrashDest can t = do
  fid <- U.nextRandom
  return joinPath [buildTrashedAtPath can t, U.toString fid, trashContainerName]

generate :: IO ()
generate = do
  wrappedCan <- findTrashCanLocation
  case wrappedCan of
    Right can -> do
      s <- datasize
      ts <- replicateM s trash
      forM_ ts (\(lt, tr) -> trashToCan can [tr] lt)
    Left _ -> print "Trash can location cannot be detected ."

main :: IO ()
main = generate >> print "done"
