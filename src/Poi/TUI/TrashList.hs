module Poi.TUI.TrashList (render) where

import qualified Brick.AttrMap as A
import Brick.Types (Widget)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (hLimit, str, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import Poi.Entity (OrderedTrashCan, Trash (trashOriginalPath))

trashedItemListAttr :: A.AttrName
trashedItemListAttr = L.listSelectedAttr <> A.attrName "trashedItemList"

trashedItemList :: (Show a) => Bool -> a -> Widget ()
trashedItemList sel a =
  let selStr s =
        if sel
          then withAttr trashedItemListAttr (str $ "<" <> s <> ">")
          else str s
   in str "Item " <+> selStr (show a)

makeRow :: Trash -> Widget ()
makeRow = str . trashOriginalPath

render :: L.List () OrderedTrashCan -> Widget ()
render items =
  B.borderWithLabel (str "trashed items") $
    hLimit 100 $
      vLimit 100 $
        L.renderList trashedItemList True items
