module Poi.Action where

data PoiActionError
  = PoiDeleteError
  | PoiMoveError
  | PoiRollbackError
  deriving (Show, Eq)

type PoiActionResult a = Either PoiActionError a
