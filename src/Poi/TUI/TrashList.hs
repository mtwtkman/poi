module Poi.TUI.TrashList (
  handleEvent,
  render,
) where

import qualified Graphics.Vty as V
import qualified Brick.AttrMap as A
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget, zoom)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (Padding (Max), padBottom, padRight, str, withAttr)
import qualified Brick.Widgets.List as L
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as Vec
import Lens.Micro ((^.))
import Poi.Entity (Trash (Trash), TrashCanLocation (TrashCanLocation))
import Poi.TUI.Common (Name)
import Poi.TUI.State (State, currentTrashes, trashCanLocation)
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
makeRow s i (Trash name root _ trashedAt) =
  s $ show i <> ". " <> formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" trashedAt <> ": " <> joinPath [root, name]

handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent (VtyEvent e) = do
  let l = zoom currentTrashes
  case e of
    V.EvKey V.KEnter [] -> l $ L.handleListEvent e
    _ -> l $ L.handleListEvent e
handleEvent _ = return ()

render :: State -> Widget Name
render st =
  let
    TrashCanLocation can = st ^. trashCanLocation
    ts = st ^. currentTrashes
   in
    B.borderWithLabel (str $ "Trash can path: " <> can) $
      if Vec.null (L.listElements ts)
        then padRight Max $ padBottom Max $ str "Trash can is empty."
        else L.renderListWithIndex trashedItemList True ts
