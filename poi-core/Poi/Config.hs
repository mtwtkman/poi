module Poi.Config where

data Config = Config
  { configTrashCanPath :: !FilePath
  , configRetentionDays :: !Int
  , configConflictPrefix :: !String
  }
  deriving (Show, Eq)
