{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Poi.Test.Arbitrary where

import Data.Fixed (Pico, mod')
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Ratio ((%))
import Data.Time (
  Day (ModifiedJulianDay),
  DiffTime,
  LocalTime (LocalTime),
  TimeOfDay (TimeOfDay),
  TimeZone (TimeZone),
  defaultTimeLocale,
  formatTime,
  fromGregorian,
  minutesToTimeZone,
  timeToTimeOfDay,
  toGregorian,
 )
import Poi.Entity (TrashCanLocation (TrashCanLocation))
import System.Random (Random)
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Gen, choose, listOf1)

instance Arbitrary TrashCanLocation where
  arbitrary = TrashCanLocation . (<>) "/" <$> listOf1 arbitrary

deriving instance Random Day

supportedDayRange :: (Day, Day)
supportedDayRange = (fromGregorian 1970 1 1, fromGregorian 9999 12 31)

instance Arbitrary Day where
  arbitrary = choose supportedDayRange
  shrink day =
    let
      (y, m, d) = toGregorian day
      dayShrink =
        ([fromGregorian y m (d - 1) | d > 1])
      monthShrink =
        ([fromGregorian y (m - 1) d | m > 1])
      yearShrink
        | y > 2000 = [fromGregorian (y - 1) m d]
        | y < 2000 = [fromGregorian (y + 1) m d]
        | otherwise = []
     in
      dayShrink ++ monthShrink ++ yearShrink

reduceDigits :: Int -> Pico -> Maybe Pico
reduceDigits (-1) _ = Nothing
reduceDigits n x =
  let
    d :: Pico
    d = 10 ^^ negate n
    r = mod' x d
   in
    case r of
      0 -> reduceDigits (n - 1) x
      _ -> Just $ x - r

instance Arbitrary DiffTime where
  arbitrary = picosecondsToDiffTime' <$> choose (0, 86400 * 10 ^ (12 :: Int))
   where
    picosecondsToDiffTime' :: Integer -> DiffTime
    picosecondsToDiffTime' x = fromRational (x % 10 ^ (12 :: Int))

instance Arbitrary TimeOfDay where
  arbitrary = fmap timeToTimeOfDay arbitrary
  shrink (TimeOfDay h m s) =
    let
      shrinkInt 0 = []
      shrinkInt 1 = [0]
      shrinkInt _ = [0, 1]
      shrinkPico 0 = []
      shrinkPico 1 = [0]
      shrinkPico p = case reduceDigits 12 p of
        Just p' -> [0, 1, p']
        Nothing -> [0, 1]
     in
      [TimeOfDay h' m s | h' <- shrinkInt h]
        ++ [TimeOfDay h m' s | m' <- shrinkInt m]
        ++ [TimeOfDay h m s' | s' <- shrinkPico s]

instance Arbitrary LocalTime where
  arbitrary = LocalTime <$> arbitrary <*> arbitrary
  shrink (LocalTime d tod) = [LocalTime d' tod | d' <- shrink d] ++ [LocalTime d tod' | tod' <- shrink tod]

instance Arbitrary TimeZone where
  arbitrary = minutesToTimeZone <$> choose (-720, 720)
  shrink (TimeZone 0 _ _) = []
  shrink (TimeZone _ s n) = [TimeZone 0 s n]

data TrashedFilePath = TrashedFilePath
  { zt :: LocalTime
  , tString :: String
  , filename :: String
  , labeledPath :: String
  , isDir :: Bool
  }
  deriving (Show)

instance Arbitrary TrashedFilePath where
  arbitrary = do
    t <- arbitrary :: Gen LocalTime
    f <- (listOf1 arbitrary :: Gen String) <&> show
    let
      ts = formatTime defaultTimeLocale "%FT%H:%M:%S.%q%z" t
      lp = intercalate "/" [ts, f]
    TrashedFilePath t ts f lp <$> arbitrary
