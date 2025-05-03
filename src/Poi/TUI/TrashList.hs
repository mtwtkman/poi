module Poi.TUI.TrashList (
  handleEvent,
  render,
) where

import qualified Brick.AttrMap as A
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget, zoom)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (str, withAttr)
import Brick.Widgets.List (GenericList (listElements))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Poi.Entity (Trash (Trash))
import Poi.TUI.Common (Name)
import Poi.TUI.State (State, currentTrashes)
import System.FilePath (joinPath)

trashedItemListAttr :: A.AttrName
trashedItemListAttr = L.listSelectedAttr <> A.attrName "trashedItemList"

trashedItemList :: Int -> Bool -> Trash -> Widget Name
trashedItemList i sel t =
  let selStr =
        if sel
          then withAttr trashedItemListAttr . str
          else str
   in makeRow selStr (i + 1) t

makeRow :: (String -> Widget Name) -> Int -> Trash -> Widget Name
makeRow s i (Trash name root _ _) =
  s $ show i <> ": " <> joinPath [root, name]

handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent (VtyEvent e) =
  let l = zoom currentTrashes
   in case e of
        V.EvKey (V.KChar 'b') [V.MCtrl] -> l L.listMovePageUp
        V.EvKey (V.KChar 'f') [V.MCtrl] -> l L.listMovePageDown
        _ -> l $ L.handleListEvent e
handleEvent _ = return ()

render :: State -> Widget Name
render st =
  let l = st ^. currentTrashes
      items = listElements l
   in B.borderWithLabel (str $ "Trashes: " <> show (Vec.length items)) $
        L.renderListWithIndex trashedItemList True l
