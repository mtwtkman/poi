module Poi.TUI.Development.DebugWindow (render, style)
where

import Brick (AttrName, Padding (Max), attrName, padRight, str, txt, vBox, vLimit, withAttr)
import Brick.Types (Widget)
import Brick.Util (on)
import Brick.Widgets.Border (borderWithLabel)
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Graphics.Vty (Attr, brightRed, white)
import Lens.Micro ((^.))
import qualified Poi.Internal.Data.Text as T
import Poi.TUI.Common (Name)
import Poi.TUI.State (State, filterCriteria)

attr :: AttrName
attr = attrName "debugWindow"

style :: [(AttrName, Attr)]
style =
  [ (attr, white `on` brightRed)
  ]

render :: State -> Widget Name
render st =
  let criteria = T.concatText (E.getEditContents (st ^. filterCriteria))
   in borderWithLabel (str "DEBUG WINDOW") $
        withAttr attr $
          vLimit 3 $
            padRight Max $
              vBox
                [ txt (T.pack "filter criteria: " <> criteria)
                , str ("filter criteria length: " <> show (T.length criteria))
                ]
