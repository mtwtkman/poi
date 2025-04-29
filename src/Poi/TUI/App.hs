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
import qualified Graphics.Vty as V
import Poi.TUI.State (State, initialState)
import qualified Poi.TUI.TrashList as TrashList
import Control.Monad (void)

drawUI :: State -> [Widget () ]
drawUI st = [ui]
 where
  trashList = TrashList.render st
  ui =
    C.vCenter $
      vBox
        [ C.hCenter trashList
        ]

appEvent :: T.BrickEvent () e -> T.EventM () State ()
appEvent ev@(T.VtyEvent e) =
  case e of
    V.EvKey (V.KChar 'q') [] -> M.halt
    _ -> TrashList.handleEvent ev
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
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

app :: M.App State e ()
app =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = M.showFirstCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return ()
    , M.appAttrMap = const theMap
    }

start :: IO ()
start = do
  s <- initialState
  void $ M.defaultMain app s
