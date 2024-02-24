module Poi.Type.File where

import Data.Vector (Vector)

data File
  = File !FilePath
  | Directory !FilePath !(Vector File)
  deriving (Show, Eq)

doesFileExist :: File -> FilePath -> Bool
doesFileExist (File f) = (==) f
doesFileExist (Directory f _) = (==) f
