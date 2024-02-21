module Poi.Type.File where

data File =
    File !FilePath
  | Directory !FilePath ![File]
  deriving (Show, Eq)
