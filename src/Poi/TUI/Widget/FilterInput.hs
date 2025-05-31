{-# LANGUAGE OverloadedStrings #-}

module Poi.TUI.Widget.FilterInput (
  handleEvent,
  render,
  cursor,
) where

import Control.Applicative ((<|>))
import qualified Brick.Focus as F
import Brick.Types (BrickEvent, CursorLocation, EventM, Widget, get, modify, zoom)
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core (str, txt, vLimit)
import Brick.Widgets.Edit (getEditContents)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Data.Vector as Vec
import Lens.Micro ((^.))
import Poi.Entity (Trash (Trash))
import Poi.Internal.Data.Text (concatText)
import Poi.TUI.Common (Name)
import Poi.TUI.State (ListItem (ListItem), State, filterCriteria, filterInputFocus, match, trashList, unmatch)
import System.FilePath (joinPath)
import qualified Text.Fuzzy as F

handleEvent :: BrickEvent Name e -> EventM Name State ()
handleEvent e = do
  zoom filterCriteria $ E.handleEditorEvent e
  st <- get
  let criteria = concatText $ getEditContents $ st ^. filterCriteria
      ts = L.listElements $ st ^. trashList
      sel = L.listSelectedElement $ st ^. trashList
      newL =
        if T.null criteria
          then Vec.map match ts
          else filterTrashesByPath criteria ts
      newPos = case sel of
        Nothing -> Just 0
        Just (current, x) ->
          let targets = Vec.filter (\(ListItem _ _ matched _) -> matched) ts
           in Vec.elemIndex x targets <|> Just current
  zoom trashList $ modify $ L.listReplace newL newPos

filterTrashesByPath :: T.Text -> Vec.Vector ListItem -> Vec.Vector ListItem
filterTrashesByPath p = go p Vec.empty
 where
  go :: T.Text -> Vec.Vector ListItem -> Vec.Vector ListItem -> Vec.Vector ListItem
  go ptn acc xs
    | Vec.null xs = acc
    | otherwise =
        let x@(ListItem (Trash name root _ _) _ matched _) = Vec.head xs
            rest = Vec.tail xs
         in Vec.concat
              [ Vec.singleton $
                  case F.match ptn (joinPath [root, name]) "" "" T.pack False of
                    Just _ -> if matched then x else match x
                    Nothing -> if matched then unmatch x else x
              , go ptn acc rest
              ]

cursor :: State -> [CursorLocation Name] -> Maybe (CursorLocation Name)
cursor = F.focusRingCursor (^. filterInputFocus)

inputRow :: [T.Text] -> Widget Name
inputRow = txt . concatText

render :: State -> Widget Name
render st =
  B.borderWithLabel (str "Filter by filepath") $
    vLimit 1 $
      F.withFocusRing
        (st ^. filterInputFocus)
        (E.renderEditor inputRow)
        (st ^. filterCriteria)
