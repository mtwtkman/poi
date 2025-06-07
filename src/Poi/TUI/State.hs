{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Poi.TUI.State (
  State (..),
  initialState,
  visibleTrashList,
  trashList,
  trashCanLocation,
  filterCriteria,
  filterInputFocus,
  ListItem (..),
  mark,
  unmark,
  toggleMark,
  updateListItem,
)
where

import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import Lens.Micro ((&), (.~), (^.))
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

type Marked = Bool

data ListItem = ListItem Trash Marked

mark :: ListItem -> ListItem
mark (ListItem t _) = ListItem t True

unmark :: ListItem -> ListItem
unmark (ListItem t _) = ListItem t False

instance Eq ListItem where
  ListItem a _ == ListItem b _ = a == b

toggleMark :: ListItem -> ListItem
toggleMark
  (ListItem t m) = ListItem t (not m)

data State = State
  { _visibleTrashList :: L.List Name ListItem
  , _trashList :: V.Vector ListItem
  , _filterCriteria :: E.Editor T.Text Name
  , _filterInputFocus :: F.FocusRing Name
  , _trashCanLocation :: TrashCanLocation
  }

makeLenses ''State

initialState :: IO State
initialState = do
  foundCan <- findTrashCanLocation
  case foundCan of
    Right can -> do
      result <- listUp Asc can
      case result of
        Right (OrderedTrashCan trashes _) ->
          let l = buildItemList trashes
           in return $
                State
                  (L.list PoiTrashList l 0)
                  l
                  (E.editorText FilterPatternInput Nothing "")
                  (F.focusRing [FilterPatternInput])
                  can
        Left e -> error $ show e
    Left e -> error $ show e
 where
  buildItemList :: [Trash] -> V.Vector ListItem
  buildItemList ts = V.fromList [ListItem t False | t <- ts]

updateListItem :: State -> ListItem -> State
updateListItem st x = updateVisibleListItem (updateOriginalListItem st x) x
 where
  update :: V.Vector ListItem -> ListItem -> V.Vector ListItem
  update l new =
    case V.findIndex (== new) l of
      Just i -> l V.// [(i, new)]
      Nothing -> l

  updateOriginalListItem :: State -> ListItem -> State
  updateOriginalListItem st x = st & trashList .~ update (st ^. trashList) x

  updateVisibleListItem :: State -> ListItem -> State
  updateVisibleListItem st x =
    let vl = st ^. visibleTrashList
     in st & visibleTrashList .~ L.listReplace (update (L.listElements vl) x) (L.listSelected vl) vl
