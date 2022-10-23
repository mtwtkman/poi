module Poi.Operation where

data PoiOperationError
  = PoiDeleteError
  | PoiMoveError
  | PoiRollbackError
  deriving (Show, Eq)

type PoiOperationResult a = Either PoiOperationError a
