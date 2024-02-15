{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Poi.Control.Monad.Free (Free (..))
import Poi.Control.Monad.Trans.Reader (Reader, ask, runReader)
import System.Exit (exitSuccess)

data OpF a = GetLine !(String -> a) | PutStrLn !String !a | Exit

instance Functor OpF where
  fmap f (GetLine k) = GetLine (f . k)
  fmap f (PutStrLn s a) = PutStrLn s (f a)
  fmap _ Exit = Exit

type Op = Free OpF

runOpIO :: Op a -> IO a
runOpIO (Pure a) = return a
runOpIO (Impure (GetLine f)) = getLine >>= runOpIO . f
runOpIO (Impure (PutStrLn s a)) = putStrLn s >> runOpIO a
runOpIO (Impure Exit) = exitSuccess

calculateContentLen :: Reader String Int
calculateContentLen = do length <$> ask

main :: IO ()
main = do
  print (runReader calculateContentLen "hi")
