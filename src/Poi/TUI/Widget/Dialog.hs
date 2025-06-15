module Poi.TUI.Widget.Dialog (
  render,
  confirmation
) where

import Brick (Widget, padAll, str)
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (hCenter)
import qualified Brick.Widgets.Dialog as D
import Poi.TUI.Common (Name (ConfirmationDialog))
import Poi.TUI.State (Answer (OK, Cancel), DialogState (DialogState))

render :: DialogState -> Widget Name
render (DialogState d msg) =
  D.renderDialog d $
    border $
      hCenter $
        padAll 1 $
          str msg

confirmation :: Maybe String -> String -> DialogState
confirmation title =
  DialogState
    ( D.dialog
        (fmap str title)
        ( Just
            ( ConfirmationDialog
            ,
              [ ("Ok", ConfirmationDialog, OK)
              , ("Cancel", ConfirmationDialog, Cancel)
              ]
            )
        )
        0
    )
