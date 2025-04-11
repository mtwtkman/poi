module Poi.Action (PoiAction (..)) where

data PoiAction
  = ListUp
  | Toss [FilePath]
  | PickUpByIndex [Int]
  | EmptyTrashCan
  | DeleteDayBefore Int
  | DeleteByIndex [Int]
  | StartTuiApplication
  | ShowVersion
  deriving (Show, Eq)
