module Poi.Monad where

class Monad m => PoiMonad m where
  moveFile :: FilePath -> m ()
  checkFilePath :: FilePath -> m Bool
