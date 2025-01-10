module Poi.Version (version, Version) where

import Data.List (intercalate)

data Version = Version
  { major :: Int
  , minor :: Int
  , micro :: Int
  }
  deriving (Eq)

instance Show Version where
  show (Version{major = x, minor = y, micro = z}) = intercalate "." $ map show [x, y, z]

version :: Version
version = Version 0 0 1
