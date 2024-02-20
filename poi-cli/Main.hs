{-# LANGUAGE FlexibleInstances #-}

module Main where

import Poi.Control.Monad.Trans.Reader (Reader, ask, runReader)
import Prelude hiding (readFile)
import qualified Prelude

class (Monad m) => FSMonad m where
  readFile :: FilePath -> m String

numCharactersInFile :: (FSMonad m) => FilePath -> m Int
numCharactersInFile filename = do
  contents <- readFile filename
  return (length contents)

instance FSMonad IO where
  readFile = Prelude.readFile

data MockFS = SingleFile !FilePath !String

instance FSMonad (Reader MockFS) where
  readFile fp = do
    (SingleFile fp' contents) <- ask
    if fp == fp'
      then return contents
      else error "file not found"

test :: Int
test = runReader (numCharactersInFile "test.txt") (SingleFile "test.txt" "hogehoge")

main :: IO ()
main = do
  x <- numCharactersInFile "poi-cli/README.md"
  print $ show x
  print $ show test

-- numCharactersInFile "test.txt"
