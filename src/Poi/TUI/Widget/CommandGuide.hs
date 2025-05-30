module Poi.TUI.Widget.CommandGuide (
  render,
  style,
) where

import Brick (AttrName, Padding (Max, Pad), Widget, attrName, on, padLeft, padRight, str, vLimit, withAttr)
import Brick.Widgets.Core (hBox)
import Graphics.Vty (Attr, black, white)
import Poi.TUI.Common (Name)

labelAttr :: AttrName
labelAttr = attrName "guideLabel"

style :: [(AttrName, Attr)]
style =
  [ (labelAttr, black `on` white)
  ]

guide :: String -> String -> Widget Name
guide k c =
  hBox
    [ withAttr labelAttr $ str k
    , str (" " <> c)
    ]

succGuide :: String -> String -> Widget Name
succGuide k = padLeft (Pad 3) . guide k

render :: Widget Name
render =
  vLimit 1 $
    padRight Max $
      hBox
        [ guide "ctrl-x" "Delete"
        , succGuide "ctrl-r" "Rollback"
        , succGuide "ctrl-f" "Page down"
        , succGuide "ctrl-b" "Page up"
        , succGuide "tab" "Mark"
        , succGuide "ctrl-c" "Exit"
        ]
