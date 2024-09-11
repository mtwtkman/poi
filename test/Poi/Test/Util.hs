module Poi.Test.Util (
  pickRandom,
) where

import System.Random (Random (randomR), getStdRandom)

pickRandom :: [a] -> IO (Int, a)
pickRandom src = do
  i <- getStdRandom (randomR (0, length src - 1)) :: IO Int
  return (i, src !! i)
