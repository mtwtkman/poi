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
import Poi.TUI.Common (Name)
import Poi.TUI.State (State, filterCriteria, filterInputFocus)
import Poi.Entity (Trash)

handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent e = zoom filterCriteria $ E.handleEditorEvent e

filterTrashesByPath :: T.Text -> [Trash] -> [Trash]
filterTrashesByPath t xs = undefined

cursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
cursor = F.focusRingCursor (^. filterInputFocus)

inputRow :: [T.Text] -> Widget Name
inputRow = txt . T.unlines

render :: State -> Widget Name
render st =
  B.borderWithLabel (str "Filter") $
    vLimit 1 $
      F.withFocusRing
        (st ^. filterInputFocus)
        (E.renderEditor inputRow)
        (st ^. filterCriteria)
