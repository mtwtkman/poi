{-# LANGUAGE FlexibleInstances #-}

module Poi.Action where

import Data.Time.LocalTime (LocalTime)
import Poi.Control.Monad.Trans.Reader (Reader, ask)
import Poi.Type.Result (Result)
import Poi.Type.Time (TimeUnit)
import Poi.Type.File (File)

class (Monad m) => PoiMonad m where
  moveFile :: File -> File -> m (Result ())
  listDirectory :: File -> m (Result [File])
  deleteFile :: File -> m (Result ())
  displayMessages :: a -> m String
  searchFile :: File -> String -> m (Maybe [File])
  doesExistFile :: File -> m Bool

  getCurrentLocalDateTime :: m LocalTime
  getPastLocalDateTime :: Int -> TimeUnit -> m LocalTime

data PoiPure = PoiPure
  { poiPureDir :: ![File]
  , poiPureTargetDir :: ![File]
  , poiPurePoiDir :: ![File]
  , poiPureCurrentLocalDateTime :: !LocalTime
  , poiPureConsoleBuffer :: !(Maybe String)
  }
  deriving (Show, Eq)

instance PoiMonad (Reader PoiPure) where
  moveFile src dest = do
    env <- ask
    return $ Right ()
