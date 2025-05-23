module Poi.TUI.Widget.TrashList (
  handleEvent,
  render,
  style,
) where

import Brick (emptyWidget, on)
import Brick.AttrMap (AttrName, attrName)
import Brick.Types (BrickEvent (VtyEvent), EventM, Widget, get, modify, zoom)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (Padding (Max), padBottom, padRight, str, withAttr)
import qualified Brick.Widgets.List as L
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Vector as Vec
import Graphics.Vty (Attr, black, white)
import qualified Graphics.Vty as V
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use)
import Poi.Action.Bury (deleteTrashByIndices)
import Poi.Entity (Trash (Trash), TrashCanLocation (TrashCanLocation))
import Poi.TUI.Common (Name)
import Poi.TUI.State (ListItem (ListItem), State, toggleMark, trashCanLocation, trashList)
import System.FilePath (joinPath)

trashedItemListAttr :: AttrName
trashedItemListAttr = L.listSelectedAttr <> attrName "trashedItemList"

style :: [(AttrName, Attr)]
style =
  [ (trashedItemListAttr, black `on` white)
  ]

trashedItemList :: Int -> Bool -> ListItem -> Widget Name
trashedItemList i sel t =
  let selStr =
        if sel
          then withAttr trashedItemListAttr . str
          else str
   in makeRow selStr (i + 1) t

makeRow :: (String -> Widget Name) -> Int -> ListItem -> Widget Name
makeRow s i (ListItem (Trash name root _ trashedAt) marked True) =
  s $ (if marked then "*" else " ") <> " " <> show i <> ". " <> formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" trashedAt <> ": " <> joinPath [root, name]
makeRow _ _ (ListItem _ _ False) =
  emptyWidget

buryTrash :: EventM Name State ()
buryTrash = do
  st <- get
  liftIO $ do
    res <- deleteTrashByIndices (st ^. trashCanLocation) (indicies $ L.listElements (st ^. trashList))
    case res of
      Right done -> return ()
      Left err -> return ()
 where
  indicies :: Vec.Vector ListItem -> [Int]
  indicies xs = Vec.toList $ Vec.map fst (Vec.filter (\(_, ListItem _ m _) -> m) $ Vec.zip (Vec.fromList [1 ..]) xs)

pickUpTrash :: EventM Name State ()
pickUpTrash = undefined
 where
  pickUpSelected :: EventM Name State ()
  pickUpSelected = undefined

  pickUpMarked :: EventM Name State ()
  pickUpMarked = undefined

updateMark :: Int -> ListItem -> EventM Name State ()
updateMark i t = do
  l <- use trashList
  let newL = Vec.map (\x -> if t == x then toggleMark x else x) (L.listElements l)
  zoom trashList $ modify $ L.listReplace newL (Just i)

handleEvent :: BrickEvent n e -> EventM Name State ()
handleEvent (VtyEvent e) = do
  case e of
    V.EvKey (V.KChar 'x') [V.MCtrl] -> buryTrash
    V.EvKey (V.KChar 'r') [V.MCtrl] -> pickUpTrash
    V.EvKey (V.KChar '\t') [] -> do
      l <- use trashList
      case L.listSelectedElement l of
        Just (i, t) -> updateMark i t
        Nothing -> return ()
    _ -> zoom trashList $ L.handleListEvent e
handleEvent _ = return ()

render :: State -> Widget Name
render st =
  let
    TrashCanLocation can = st ^. trashCanLocation
    ts = st ^. trashList
   in
    B.borderWithLabel (str $ "Trash can path: " <> can) $
      if Vec.null (L.listElements ts)
        then padRight Max $ padBottom Max $ str "Trash can is empty."
        else L.renderListWithIndex trashedItemList True ts
