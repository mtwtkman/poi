module Poi.TUI.Common (Name (..), TuiResult(..)) where

import Poi.Action.Type.Result (PoiActionResult)

data Name
  = PoiTrashList
  | FilterPatternInput
  | CommandGuide
  | ConfirmationDialog
  deriving (Eq, Ord, Show)

data TuiResult a b = PoiActionResult (PoiActionResult a) | BrickEventResult b
