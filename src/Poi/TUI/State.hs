{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Poi.TUI.State (
  State (..),
  initialState,
  trashList,
  trashCanLocation,
  filterCriteria,
  filterInputFocus,
  ListItem (ListItem),
  match,
  unmatch,
  mark,
  unmark,
  toggleMark,
  fetchVisibleMarkedTrashes,
)
where

import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.List as Li
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

type Marked = Bool
type Matched = Bool

data ListItem = ListItem Trash Marked Matched

match :: ListItem -> ListItem
match (ListItem t m _) = ListItem t m True

unmatch :: ListItem -> ListItem
unmatch (ListItem t m _) = ListItem t m False

mark :: ListItem -> ListItem
mark (ListItem t _ m) = ListItem t True m

unmark :: ListItem -> ListItem
unmark (ListItem t _ m) = ListItem t False m

instance Eq ListItem where
  ListItem a _ _ == ListItem b _ _ = a == b

toggleMark :: ListItem -> ListItem
toggleMark (ListItem t marked matched) = ListItem t (not marked) matched

data State = State
  { _trashList :: L.List Name ListItem
  , _filterCriteria :: E.Editor T.Text Name
  , _filterInputFocus :: F.FocusRing Name
  , _trashCanLocation :: TrashCanLocation
  }

fetchVisibleMarkedTrashes :: State -> V.Vector ListItem
fetchVisibleMarkedTrashes (State xs _ _ _) = V.filter (\(ListItem _ mk mt) -> mk && mt) (L.listElements xs)

makeLenses ''State

initialState :: IO State
initialState = do
  foundCan <- findTrashCanLocation
  case foundCan of
    Right can -> do
      result <- listUp Asc can
      case result of
        Right (OrderedTrashCan trashes _) ->
          return $
            State
              (initialCurrentTrashes trashes)
              initialFilterCriteia
              (F.focusRing [FilterPatternInput])
              can
        Left e -> error $ show e
    Left e -> error $ show e
 where
  initialCurrentTrashes trashes = L.list PoiTrashList (V.fromList (Li.map (\t -> ListItem t False True) trashes)) 10
  initialFilterCriteia = E.editorText FilterPatternInput Nothing ""
