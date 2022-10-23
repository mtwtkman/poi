module Poi.Operation where

data PoiOperationError = PoiDeleteError deriving (Show, Eq)

type PoiOperationResult a = Either PoiOperationError a
