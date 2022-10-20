module Poi.Operation.List where

import Poi.Entity

listAll :: TrashBox -> IO String
listAll tb = readFile $ metaInfoLocation tb
