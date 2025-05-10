{-# LANGUAGE OverloadedStrings #-}

module Poi.TUI.FilterInput (
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
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Lens.Micro ((^.))
import Poi.Entity (Trash (Trash))
import Poi.TUI.Common (Name)
import Poi.TUI.State (State, allTrashes, filterCriteria, filterInputFocus, trashList)
import System.FilePath (joinPath)
import Text.Fuzzy (match)
import Poi.Internal.Data.Text (concatText)

handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent e = do
  zoom filterCriteria $ E.handleEditorEvent e
  st <- get
  let criteria = concatText $ getEditContents $ st ^. filterCriteria
      ts = st ^. allTrashes
      newL = Vec.fromList $ if T.null criteria then ts else filterTrashesByPath criteria ts
  zoom trashList $ modify $ L.listReplace newL Nothing

filterTrashesByPath :: T.Text -> [Trash] -> [Trash]
filterTrashesByPath p = go p []
 where
  go :: T.Text -> [Trash] -> [Trash] -> [Trash]
  go _ acc [] = acc
  go ptn acc (x@(Trash name root _ _) : rest) = case match ptn (joinPath [root, name]) "" "" T.pack False of
    Just _ -> x : go ptn acc rest
    Nothing -> go ptn acc rest

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
