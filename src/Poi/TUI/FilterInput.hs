{-# LANGUAGE OverloadedStrings #-}

module Poi.TUI.FilterInput (
  handleEvent,
  render,
  cursor,
) where

import qualified Brick.Focus as F
import Brick.Types (BrickEvent, CursorLocation, EventM, Widget, zoom)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (str, txt, vLimit)
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Lens.Micro ((^.))
import Poi.Entity (Trash (Trash))
import Poi.TUI.Common (Name)
import Poi.TUI.State (State, filterCriteria, filterInputFocus)
import System.FilePath (joinPath)
import Text.Regex.TDFA ((=~))

handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent e = zoom filterCriteria $ E.handleEditorEvent e

filterTrashesByPath :: T.Text -> [Trash] -> [Trash]
filterTrashesByPath t = filter (searchPath t)
 where
  searchPath :: T.Text -> Trash -> Bool
  searchPath ptn (Trash name root _ _) =
    let path = T.pack $ joinPath [root, name]
     in path =~ T.unpack ptn

cursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
cursor = F.focusRingCursor (^. filterInputFocus)

inputRow :: [T.Text] -> Widget Name
inputRow = txt . T.unlines

render :: State -> Widget Name
render st =
  B.borderWithLabel (str "Filter by filepath") $
    vLimit 1 $
      F.withFocusRing
        (st ^. filterInputFocus)
        (E.renderEditor inputRow)
        (st ^. filterCriteria)
