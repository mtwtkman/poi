module Poi.TUI.App where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (
  str,
  vBox,
  withAttr,
  (<+>),
 )
import qualified Brick.Widgets.List as L
import Control.Monad (void)
import qualified Graphics.Vty as V
import qualified Poi.TUI.FilterInput as FilterInput
import Poi.TUI.State (State, initialState)
import qualified Poi.TUI.TrashList as TrashList
import Poi.TUI.Common (Name)
import Graphics.Vty (Modifier(MCtrl))

drawUI :: State -> [Widget Name]
drawUI st = [ui]
 where
  trashList = TrashList.render st
  filterInput = FilterInput.render st
  ui =
    C.vCenter $
      vBox
        [ C.hCenter filterInput
        , C.hCenter trashList
        ]

appEvent :: T.BrickEvent Name e -> T.EventM Name State ()
appEvent ev@(T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'c') [MCtrl] -> M.halt
    V.EvKey V.KUp [] -> TrashList.handleEvent ev
    V.EvKey V.KDown [] -> TrashList.handleEvent ev
    V.EvKey (V.KChar 'b') [MCtrl] -> TrashList.handleEvent ev
    V.EvKey (V.KChar 'f') [MCtrl] -> TrashList.handleEvent ev
    _ -> FilterInput.handleEvent ev
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget Name
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str $ "<" <> s <> ">")
          else str s
   in C.hCenter $ str "Item " <+> selStr (show a)

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listSelectedAttr, V.black `on` V.white)
    , (customAttr, fg V.cyan)
    ]

app :: M.App State e Name
app =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = FilterInput.cursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return ()
    , M.appAttrMap = const theMap
    }

start :: IO ()
start = do
  s <- initialState
  void $ M.defaultMain app s
