module Poi.Action where

data ErrorReason = SomethingWrong | ExistsSamePath deriving (Show, Eq)

data PoiActionError
  = PoiDeleteError
  | PoiMoveError
  | PoiRollbackError ErrorReason
  deriving (Show, Eq)

type PoiActionResult a = Either PoiActionError a
