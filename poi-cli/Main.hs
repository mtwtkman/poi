{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Exception
import Poi.Control.Monad.Trans.Reader (Reader, ask, runReader)
import System.Directory
import System.IO.Error (isDoesNotExistError)
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

newtype X a = X a deriving (Show, Eq)

xx :: (Monad m) => a -> Bool -> m (Either () a)
xx x b
  | b = return $ Right x
  | otherwise = return $ Left ()

instance Functor X where
  fmap f (X a) = X (f a)

instance Applicative X where
  pure = X
  X f <*> X a = X (f a)

instance Monad X where
  X a >>= k = k a

class (Monad m) => Hoge m where
  hoge :: m a -> Bool -> m a

instance Hoge X where
  hoge (X a) b = do
    _ <- xx a True
    _ <- xx a False
    _ <- xx a b
    return a

fun :: (Monad m) => m (Either () Int)
fun = do
  res <- return (Right 1) >> return (Left ()) >> return (Right 10)
  case res of
    Right x -> return $ Right (x * 10)
    Left () -> return $ Left ()

main :: IO ()
main = do
  con <- (try $ renameFile "a" "aaa" :: IO (Either IOError ()))
  case con of
    Right x -> print $ show x
    Left e -> do
      if isDoesNotExistError e
        then print "ouch"
        else print $ show e
