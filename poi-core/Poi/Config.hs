module Poi.Config where

data Config = Config
  { configTrashCanPath :: !FilePath
  , configRetentionDays :: !Int
  , configConflictPrefix :: !String
  }
  deriving (Show, Eq)

defaultConfig :: Config
defaultConfig =
  Config "~/.poi/TRASH-CAN" 90 "poi-restore_"
