module Main where

import Control.Monad (forM_, replicateM)
import Data.Functor ((<&>))
import Data.Time (LocalTime, addLocalTime, secondsToNominalDiffTime)
import Poi.Entity (
  TrashCanLocation,
  TrashDirectoryStructureInfo (TrashDirectoryStructureInfo),
  trashDirectoryStructureInfo,
 )
import Poi.File.IO (findTrashCanLocation, saveParentLocation)
import Poi.Time (getCurrent)
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath (joinPath)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)

datasize :: IO Int
datasize = do
  let defaultSize = 50 :: Int
  env <- lookupEnv "POI_GENDATA_SIZE"
  case env of
    Nothing -> return defaultSize
    Just "" -> return defaultSize
    Just v -> return $ fromJust $ readMaybe v <|> Just 50

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

genData :: IO (LocalTime, String, String)
genData = (,,) <$> randomLocalTime <*> (randomFilePath >>= \p -> return ("/parent/" <> p)) <*> randomFilePath

putData :: TrashCanLocation -> LocalTime -> String -> String -> IO ()
putData can t parent name = do
  (TrashDirectoryStructureInfo root container _) <- trashDirectoryStructureInfo can t
  createDirectoryIfMissing True container
  saveParentLocation root parent
  writeFile (joinPath [container, name]) ""
  return ()

main :: IO ()
main = do
  wrappedCan <- findTrashCanLocation
  case wrappedCan of
    Right can -> do
      s <- datasize
      ts <- replicateM s genData
      forM_ ts (\(t, parent, name) -> putData can t parent name)
      print $ show s <> " files created."
    Left _ -> print "Trash can location cannot be detected ."
