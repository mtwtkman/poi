{-# LANGUAGE CPP #-}

module Poi.TUI.Main where

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (
  hLimit,
  str,
  vBox,
  vLimit,
  withAttr,
  (<+>),
 )
import qualified Brick.Widgets.List as L
import Control.Monad.State (void)
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Poi.Action.ListUp (listUp)
import Poi.Entity (OrderedTrashCan (OrderedTrashCan), SortOrder (Asc), Trash)
import Poi.File.IO (findTrashCanLocation)

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
 where
  label = str "Item " <+> cur <+> str " of " <+> total
  cur = case l ^. L.listSelectedL of
    Nothing -> str "-"
    Just i -> str (show (i + 1))
  total = str $ show $ Vec.length $ l ^. L.listElementsL
  box =
    B.borderWithLabel label $
      hLimit 25 $
        vLimit 15 $
          L.renderList listDrawElement True l
  ui =
    C.vCenter $
      vBox
        [ C.hCenter box
        , str " "
        , C.hCenter $ str "Press +/- to add/remove list elements."
        , C.hCenter $ str "Press Esc to exit."
        ]

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () Trash) ()
appEvent (T.VtyEvent e) =
  case e of
    V.EvKey V.KEsc [] -> M.halt
    ev -> L.handleListEvent ev
appEvent _ = return ()

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
  let selStr s =
        if sel
          then withAttr customAttr (str $ "<" <> s <> ">")
          else str s
   in C.hCenter $ str "Item " <+> selStr (show a)

initialState :: IO (L.List () Trash)
initialState = do
  foundCan <- findTrashCanLocation
  case foundCan of
    Right can -> do
      result <- listUp Asc can
      case result of
        Right (OrderedTrashCan items) -> return $ L.list () (Vec.fromList items) 1
        Left e -> error $ show e
    Left e -> error $ show e

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (L.listAttr, V.white `on` V.blue)
    , (L.listSelectedAttr, V.blue `on` V.white)
    , (customAttr, fg V.cyan)
    ]

theApp :: M.App (L.List () Trash) e ()
theApp =
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
  void $ M.defaultMain theApp s
