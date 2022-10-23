module Poi.Action.List where

import Poi.Entity

listAll :: TrashBox -> IO String
listAll tb = readFile $ metaInfoFileLocation tb
