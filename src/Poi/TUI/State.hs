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
  ListItem (..),
  ItemDistance (..),
  match,
  unmatch,
  mark,
  unmark,
  toggleMark,
)
where

import qualified Brick.Focus as F
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.List as Li
import qualified Data.Map as M
import qualified Data.Text as T
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
data ItemDistance = ItemDistance {itemDistancePrev :: Maybe Int, itemDistanceNext :: Maybe Int}

data ListItem = ListItem
  { listItemTrash :: Trash
  , listItemMarked :: Marked
  , listItemMatched :: Matched
  , listItemDistance :: ItemDistance
  }

match :: ListItem -> ListItem
match (ListItem t m _ d) = ListItem t m True d

unmatch :: ListItem -> ListItem
unmatch (ListItem t m _ d) = ListItem t m False d

mark :: ListItem -> ListItem
mark (ListItem t _ m d) = ListItem t True m d

unmark :: ListItem -> ListItem
unmark (ListItem t _ m d) = ListItem t False m d

instance Eq ListItem where
  ListItem{listItemTrash = a} == ListItem{listItemTrash = b} = a == b

toggleMark :: ListItem -> ListItem
toggleMark
  (ListItem t mk mt d) = ListItem t (not mk) mt d

data State = State
  { _trashList :: L.List Name ListItem
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
          return $
            State
              (L.list PoiTrashList (buildItemList trashes) 0)
              (E.editorText FilterPatternInput Nothing "")
              (F.focusRing [FilterPatternInput])
              can
        Left e -> error $ show e
    Left e -> error $ show e
 where
  buildItemList :: [Trash] -> V.Vector ListItem
  buildItemList ts = V.fromList $ go [] 0 ts
   where
    initialListItem :: Trash -> ItemDistance -> ListItem

    distance :: [Trash] -> Int -> ItemDistance
    distance _ 0 = ItemDistance Nothing (Just 1)
    distance xs i = ItemDistance (Just (i - 1)) (if length xs - 1 == i then Nothing else Just (i + 1))

    go :: [ListItem] -> Int -> [Trash] -> [ListItem]
    go acc _ [] = acc
    go acc i (x : xs) = go (acc <> [initialListItem x (distance ts i)]) 1 xs
    initialListItem t = ListItem t False True
