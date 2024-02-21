module Poi.Action.Monad where

import Poi.Entity.FileObject (FileObject)
import Poi.Type.Result (Result)

class (Monad m) => PoiMonad m where
  moveFile :: FilePath -> FilePath -> m (Result ())
  listDirectory :: FilePath -> m (Result [FileObject])
  deleteFile :: FilePath -> m (Result ())
  displayMessages :: a -> m String
