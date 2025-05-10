module Poi.TUI.Development.DebugWindow (render, style)
where

import Brick (AttrMap, AttrName, Padding (Max), attrName, padRight, str, txt, vBox, vLimit, withAttr)
import Brick.Types (Widget)
import Brick.Util (on)
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Graphics.Vty (Attr, black, brightRed, white, brightBlack)
import Lens.Micro ((^.))
import Poi.TUI.Common (Name)
import Poi.TUI.State (State, filterCriteria)

attr :: AttrName
attr = attrName "debugWindow"

style :: [(AttrName, Attr)]
style =
  [ (attr, white `on` brightBlack)
  ]

render :: State -> Widget Name
render st =
  borderWithLabel (str "DEBUG WINDOW") $
    withAttr attr $
      vLimit 3 $
        padRight Max $
          vBox
            [ txt (T.pack "filter criteria: " <> T.unlines (E.getEditContents (st ^. filterCriteria)))
            ]
