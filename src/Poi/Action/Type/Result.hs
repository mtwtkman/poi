module Poi.Action.Type.Result (
  PoiActionResult,
  PoiCommonError (..),
  PoiBuryError (..),
  PoiActionError (..),
) where

import Poi.Abnormal (PoiAbnormal)
import Poi.Entity (TrashCanLocation)

data PoiCommonError
  = FileNotFound
  | TrashCanNotFound TrashCanLocation
  | IndexMustBePositive
  | IndexOverFlow
  deriving (Show, Eq)

data PoiBuryError
  = FilePathNotExist
  | BeforeDayMustBeZeroOrPositive
  deriving (Show, Eq)

data PoiActionError
  = CommonError PoiCommonError
  | PoiBuryError PoiBuryError
  | PoiTossError IOError
  deriving (Show, Eq)

type PoiActionResult a = Either PoiActionError a

instance PoiAbnormal PoiActionError
