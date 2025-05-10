{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Poi.TUI.State (
  State (..),
  initialState,
  allTrashes,
  trashList,
  trashCanLocation,
  filterCriteria,
  filterInputFocus,
)
where

import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Text as T
import qualified Data.Vector as V
import Lens.Micro.TH (makeLenses)
import Poi.Action.ListUp (listUp)
import Poi.Entity (
  OrderedTrashCan (OrderedTrashCan),
  SortOrder (Asc),
  Trash,
  TrashCanLocation,
 )
import Poi.File.IO (findTrashCanLocation)
import Poi.TUI.Common (Name (FilterPatternInput, PoiTrashList))

data State = State
  { _trashList :: L.List Name Trash
  , _allTrashes:: [Trash]
  , _filterCriteria :: E.Editor T.Text Name
  , _filterInputFocus :: F.FocusRing Name
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
        Right (OrderedTrashCan trashes order) ->
          return $
            State
              (initialCurrentTrashes trashes)
              trashes
              initialFilterCriteia
              (F.focusRing [FilterPatternInput])
              can
              order
        Left e -> error $ show e
    Left e -> error $ show e
 where
  initialCurrentTrashes trashes = L.list PoiTrashList (V.fromList trashes) 10
  initialFilterCriteia = E.editorText FilterPatternInput Nothing ""
