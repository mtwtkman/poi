module Poi.Entity.TrashedObject where

import Data.Time.Clock.System (SystemTime)

data TrashedObject = TrashedObject
  { trashedObjectPath :: !FilePath
  , trashedAt :: !SystemTime
  }
  deriving (Show, Eq)
