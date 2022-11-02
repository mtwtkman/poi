{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Poi.Test.Arbitrary where

import Control.Monad
import Data.Fixed
import Data.Ratio
import Data.Time
import Data.UUID
import Poi.Entity
import Poi.Time
import System.Random
import Test.QuickCheck

instance Arbitrary DiffTime where
  arbitrary = oneof [intSecs, fracSecs]
    where
      intSecs = secondsToDiffTime' <$> choose (0, 86400)
      fracSecs = picosecondsToDiffTime' <$> choose (0, 86400 * 10 ^ (12 :: Int))
      secondsToDiffTime' :: Integer -> DiffTime
      secondsToDiffTime' = fromInteger
      picosecondsToDiffTime' :: Integer -> DiffTime
      picosecondsToDiffTime' x = fromRational (x % 10 ^ (12 :: Int))

supportedDayRange :: (Day, Day)
supportedDayRange = (fromGregorian (-9899) 1 1, fromGregorian 9999 12 31)

deriving instance Random Day

instance Arbitrary Day where
  arbitrary = choose supportedDayRange
  shrink day =
    let (y, m, d) = toGregorian day
        dayShrink = [fromGregorian y m (d - 1) | d > 1]
        monthShrink = [fromGregorian y (m - 1) d | m > 1]
        yearShrink
          | y > 2000 = [fromGregorian (y - 1) m d]
          | y < 2000 = [fromGregorian (y + 1) m d]
          | otherwise = []
     in dayShrink ++ monthShrink ++ yearShrink

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary ObjectPath where
  arbitrary = do
    (NonEmpty value) <- arbitrary
    return (MkObjectPath value)

instance Arbitrary Timestamp where
  arbitrary = do
    (Positive value) <- arbitrary :: Gen (Positive Pico)
    return (MkTimestamp value)

instance Arbitrary TrashedAt where
  arbitrary = MkTrashedAt <$> arbitrary

instance Arbitrary UUID where
  arbitrary = choose (nil, nil)

instance Arbitrary MetaInfo where
  arbitrary = MkMetaInfo <$> arbitrary <*> arbitrary <*> arbitrary

pickMetaInfo :: RandomGen g => g -> [MetaInfo] -> (MetaInfo, g)
pickMetaInfo rnd xs =
  let len = length xs - 1
      (i, g) = randomR (0, len) rnd
   in (xs !! i, g)
