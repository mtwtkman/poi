module Poi.Action.Monad where

import Data.Time.LocalTime (LocalTime)
import Poi.Type.Result (Result)
import Poi.Type.Time (TimeUnit)

class (Monad m) => PoiMonad m where
  moveFile :: FilePath -> FilePath -> m (Result ())
  listDirectory :: FilePath -> m (Result [FilePath])
  deleteFile :: FilePath -> m (Result ())
  displayMessages :: a -> m String
  searchFile :: FilePath -> String -> m (Maybe [FilePath])
  doesExistFile :: FilePath -> m Bool
  getCurrentLocalDateTime :: m LocalTime
  getPastLocalDateTime :: Int -> TimeUnit -> m LocalTime
