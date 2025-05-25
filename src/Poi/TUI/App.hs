{-# LANGUAGE CPP #-}
module Poi.TUI.App where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Widgets.Core (vBox)
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Poi.TUI.Widget.CommandGuide as CommandGuide
import Poi.TUI.Common (Name)
import qualified Poi.TUI.Widget.Internal.DebugWindow as DebugWindow
import qualified Poi.TUI.Widget.FilterInput as FilterInput
import Poi.TUI.State (State, initialState)
import qualified Poi.TUI.Widget.TrashList as TrashList

data Mode = Debug | Release

isDebug :: Mode -> Bool
isDebug Debug = True
isDebug Release = False

drawUI :: Mode -> State -> [Widget Name]
drawUI mode st = [ui]
 where
  ui =
      vBox
        ( [ FilterInput.render st
          , TrashList.render st
          , CommandGuide.render
          ]
            <> ([DebugWindow.render st | isDebug mode])
        )

appEvent :: T.BrickEvent Name e -> T.EventM Name State ()
appEvent ev@(T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'c') [V.MCtrl] -> M.halt
    V.EvKey V.KEsc [] -> M.halt
    V.EvKey V.KUp [] -> handleTrashList
    V.EvKey V.KDown [] -> handleTrashList
    V.EvKey (V.KChar 'b') [V.MCtrl] -> handleTrashList
    V.EvKey (V.KChar 'f') [V.MCtrl] -> handleTrashList
    V.EvKey (V.KChar 'x') [V.MCtrl] -> handleTrashList
    V.EvKey (V.KChar 'r') [V.MCtrl] -> handleTrashList
    V.EvKey (V.KChar '\t') [] -> handleTrashList
    _ -> FilterInput.handleEvent ev
 where
  handleTrashList = TrashList.handleEvent ev
appEvent _ = return ()

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: Mode -> State -> A.AttrMap
theMap mode _ =
  A.attrMap
    V.defAttr
      ( TrashList.style
        <> if isDebug mode
          then DebugWindow.style
          else
            []
              <> CommandGuide.style
    )

buildMode :: Mode
buildMode =
#ifdef TUI_DEBUG
  Debug
#else
  Release
#endif

app :: M.App State e Name
app =
  M.App
    { M.appDraw = drawUI buildMode
    , M.appChooseCursor = FilterInput.cursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return ()
    , M.appAttrMap = theMap buildMode
    }

start :: IO ()
start = do
  s <- initialState
  void $ M.defaultMain app s
