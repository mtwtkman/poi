{-# LANGUAGE TemplateHaskell #-}

module Poi.TUI.State (
  State (..),
  initialState,
  currentTrashes,
  trashCanLocation,
)
where

import qualified Data.Vector as V
import Lens.Micro.TH (makeLenses)
import Poi.Action.ListUp (listUp)
import Poi.Entity (
  SortOrder (Asc),
  Trash,
  TrashCanLocation, OrderedTrashCan (OrderedTrashCan),
 )
import Poi.File.IO (findTrashCanLocation)
import qualified Brick.Widgets.List as L

data State = State
  { _currentTrashes :: L.List () Trash
  , _trashCanLocation :: TrashCanLocation
  , _currentOrder :: SortOrder
  }

makeLenses ''State

initialState :: IO State
initialState = do
  foundCan <- findTrashCanLocation
  case foundCan of
    Right can -> do
      result <- listUp Asc can
      case result of
        Right (OrderedTrashCan trashes order) -> return $ State (L.list () (V.fromList trashes) 10) can order
        Left e -> error $ show e
    Left e -> error $ show e
