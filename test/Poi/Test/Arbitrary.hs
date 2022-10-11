{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Poi.Test.Arbitrary where

import Control.Monad
import Data.Fixed
import Data.Ratio
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics
import Poi.Entity
import Poi.Time
import Test.SmallCheck.Series

newtype BoundedIntSec = BoundedIntSec Integer deriving (Eq, Ord, Show)

instance Bounded BoundedIntSec where
  minBound = BoundedIntSec 0
  maxBound = BoundedIntSec 86400

instance Num BoundedIntSec where
  (BoundedIntSec a) + (BoundedIntSec b) = BoundedIntSec (a + b)
  (BoundedIntSec a) - (BoundedIntSec b) = BoundedIntSec (a - b)
  (BoundedIntSec a) * (BoundedIntSec b) = BoundedIntSec (a * b)
  abs (BoundedIntSec a) = BoundedIntSec (abs a)
  signum (BoundedIntSec a) = BoundedIntSec (signum a)
  fromInteger = BoundedIntSec

instance Monad m => Serial m BoundedIntSec where
  series = cons1 BoundedIntSec

newtype BoundedFracSec = BoundedFracSec Integer deriving (Eq, Ord, Show)

instance Bounded BoundedFracSec where
  minBound = BoundedFracSec 0
  maxBound = BoundedFracSec (86400 * 10 ^ 12)

instance Num BoundedFracSec where
  (BoundedFracSec a) + (BoundedFracSec b) = BoundedFracSec (a + b)
  (BoundedFracSec a) - (BoundedFracSec b) = BoundedFracSec (a - b)
  (BoundedFracSec a) * (BoundedFracSec b) = BoundedFracSec (a * b)
  abs (BoundedFracSec a) = BoundedFracSec (abs a)
  signum (BoundedFracSec a) = BoundedFracSec (signum a)
  fromInteger = BoundedFracSec

instance Monad m => Serial m BoundedFracSec where
  series = cons1 BoundedFracSec

data Sec = IntSec (Positive BoundedIntSec) | FracSec (Positive BoundedFracSec)
  deriving (Eq, Show)

instance Monad m => Serial m Sec where
  series = cons1 IntSec \/ cons1 FracSec

instance Monad m => Serial m DiffTime where
  series = cons1 makeDifftime
    where
      makeDifftime :: Sec -> DiffTime
      makeDifftime (IntSec (Positive (BoundedIntSec x))) = fromInteger x
      makeDifftime (FracSec (Positive (BoundedFracSec x))) = fromRational (x % 10 ^ 12)

instance Monad m => Serial m Day where
  series = cons1 (ModifiedJulianDay . getNonNegative)

instance Monad m => Serial m UTCTime where
  series = cons2 UTCTime

instance Monad m => Serial m Timestamp where
  series = cons1 (MkTimestamp . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds)

instance Monad m => Serial m ObjectPath where
  series = newtypeCons MkObjectPath

instance Monad m => Serial m TrashedAt where
  series = newtypeCons MkTrashedAt
