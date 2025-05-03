module Poi.TUI.Common (Name(..)) where

data Name =
  PoiTrashList
  | FilterPatternInput
  deriving (Eq, Ord, Show)
