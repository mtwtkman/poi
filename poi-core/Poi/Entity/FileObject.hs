module Poi.Entity.FileObject where

newtype FileObject = FileObject FilePath
  deriving (Show, Eq)
