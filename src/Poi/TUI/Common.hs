module Poi.TUI.Common (Name(..)) where

data Name =
  PoiTrashList
  | FilterPatternInput
  | CommandSelector
  deriving (Eq, Ord, Show)
