module Poi.TUI.Widget.Confirm (render)
where

import Brick (Widget, str)
import Poi.TUI.Common (Name)
import qualified Brick.Widgets.Border as B

render :: Widget Name
render = B.border (str "hi")
