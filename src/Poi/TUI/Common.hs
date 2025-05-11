module Poi.TUI.Common (Name(..)) where

data Name =
  PoiTrashList
  | FilterPatternInput
  | CommandGuide
  deriving (Eq, Ord, Show)
