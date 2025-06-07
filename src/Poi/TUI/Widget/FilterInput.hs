{-# LANGUAGE OverloadedStrings #-}

module Poi.TUI.Widget.FilterInput (
  handleEvent,
  render,
  cursor,
) where

import qualified Brick.Focus as F
import Brick.Types (BrickEvent, CursorLocation, EventM, Widget, get, modify, zoom)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (str, txt, vLimit)
import Brick.Widgets.Edit (getEditContents)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Vector as V
import Lens.Micro ((^.))
import Poi.Entity (Trash (Trash))
import Poi.Internal.Data.Text (concatText)
import Poi.TUI.Common (Name)
import Poi.TUI.State (
  ListItem (ListItem),
  State,
  filterCriteria,
  filterInputFocus,
  trashList,
  visibleTrashList,
 )
import System.FilePath (joinPath)
import qualified Text.Fuzzy as F

handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent e = do
  zoom filterCriteria $ E.handleEditorEvent e
  st <- get
  let criteria = concatText $ getEditContents $ st ^. filterCriteria
      ts = st ^. trashList
      vs = st ^. visibleTrashList
      sel = L.listSelectedElement vs
      newVs = if T.null criteria then ts else filterTrashesByPath criteria ts
      newPos = case sel of
        Nothing -> Nothing
        Just (_, x) -> V.findIndex (== x) newVs
  zoom visibleTrashList $ modify $ L.listReplace newVs newPos

filterTrashesByPath :: T.Text -> V.Vector ListItem -> V.Vector ListItem
filterTrashesByPath ptn = V.filter filterFunc
 where
  filterFunc :: ListItem -> Bool
  filterFunc (ListItem (Trash name root _ _) _) = isJust $ F.match ptn (joinPath [root, name]) "" "" T.pack False

cursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
cursor = F.focusRingCursor (^. filterInputFocus)

inputRow :: [T.Text] -> Widget Name
inputRow = txt . concatText

render :: State -> Widget Name
render st =
  B.borderWithLabel (str "Filter by filepath") $
    vLimit 1 $
      F.withFocusRing
        (st ^. filterInputFocus)
        (E.renderEditor inputRow)
        (st ^. filterCriteria)
