module Poi.TUI.TrashList (
  handleEvent,
  render,
) where

import qualified Brick.AttrMap as A
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget, zoom)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (hLimitPercent, str, vLimitPercent, withAttr)
import Brick.Widgets.List (GenericList (listElements))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec
import Lens.Micro ((^.))
import Poi.Entity (Trash (Trash))
import Poi.TUI.State (State, currentTrashes)
import System.FilePath (joinPath)

trashedItemListAttr :: A.AttrName
trashedItemListAttr = L.listSelectedAttr <> A.attrName "trashedItemList"

trashedItemList :: Int -> Bool -> Trash -> Widget ()
trashedItemList i sel t =
  let selStr =
        if sel
          then withAttr trashedItemListAttr . str
          else str
   in makeRow selStr (i + 1) t

makeRow :: (String -> Widget ()) -> Int -> Trash -> Widget ()
makeRow s i (Trash name root _ _) =
  s $ show i <> ": " <> joinPath [root, name]

handleEvent :: BrickEvent () e -> EventM () State ()
handleEvent (VtyEvent e) = zoom currentTrashes $ L.handleListEvent e
handleEvent _ = return ()

render :: State -> Widget ()
render st =
  let l = st ^. currentTrashes
      items = listElements l
   in B.borderWithLabel (str $ "Trashes: " <> show (Vec.length items)) $
        hLimitPercent 30 $
          vLimitPercent 90 $
            L.renderListWithIndex trashedItemList True l
