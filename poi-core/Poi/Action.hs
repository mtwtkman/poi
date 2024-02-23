{-# LANGUAGE FlexibleInstances #-}

module Poi.Action where

import Control.Exception (try)
import Data.Time.LocalTime (LocalTime)
import Poi.Control.Monad.Trans.Reader (Reader, ask)
import Poi.Type.File (File)
import Poi.Type.Result (
  Error (PoiIOError),
  PoiIOError (FileNotFound, SomethingWrong),
  Result,
 )
import Poi.Type.Time (TimeUnit)
import System.Directory (renameFile)
import System.IO.Error (isDoesNotExistError)

class (Monad m) => PoiMonad m where
  moveFile :: FilePath -> FilePath -> m (Result ())
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

instance PoiMonad IO where
  moveFile src dest = do
    result <- (try $ renameFile src dest :: IO (Either IOError ()))
    case result of
      Right () -> return $ Right ()
      Left e ->
        return $
          Left $
            PoiIOError
              ( if isDoesNotExistError e
                  then FileNotFound src
                  else SomethingWrong (show e)
              )
