module Main where

import Control.Monad
import Poi
import Poi.Entity
import Poi.Object

main :: IO ()
main = do
  let box = TrashBox "sandbox/Trash"
  trashBoxCreated <- doesTrashBoxExist box
  unless trashBoxCreated $ createTrashBoxDirectory box
  path <- getLine
  trash box path
