module Poi.Prompt (
  prompt,
  confirm,
  YN (..),
  PoiPromptError (..),
  PoiPromptResult,
) where

import Data.Char (toLower)
import Data.Functor ((<&>))
import GHC.IO.Handle (hFlush)
import System.IO (stdout)
import Poi.Abnormal (PoiAbnormal)

prompt :: String -> Char -> (String -> a) -> IO a
prompt p delimiter conv = putStr (p <> [delimiter, ' ']) >> hFlush stdout >> getLine <&> conv

data PoiPromptError = InvalidInput deriving (Show, Eq)

instance PoiAbnormal PoiPromptError

type PoiPromptResult a = Either PoiPromptError a

data YN = Yes | No deriving (Eq, Show)

confirm :: String -> Char -> IO (PoiPromptResult YN)
confirm p delimiter =
  prompt
    (p <> " [Y(es)/N(o)]")
    delimiter
    ( \s -> do
        case map toLower s of
          v
            | v `elem` ["y", "yes"] -> Right Yes
            | v `elem` ["n", "no"] -> Right No
            | otherwise -> Left InvalidInput
    )
