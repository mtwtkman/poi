module Poi.Operation.Put where

import Poi.Entity
import System.Directory
import System.FilePath

moveToTrashBox :: TrashBox -> ObjectPath -> IO ()
moveToTrashBox (MkTrashBox box) (MkObjectPath src) = renameDirectory src $ box </> src
