{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

module Poi.Action where

import Control.Exception (try)
import Data.Time.LocalTime (LocalTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Poi.Control.Monad.Trans.Reader (Reader, ask)
import Poi.Type.File (File (Directory, File))
import qualified Poi.Type.File as F
import Poi.Type.Result (
  Error (PoiIOError),
  PoiIOError (FileNotFound, SomethingWrong),
  Result,
 )
import Poi.Type.Time (TimeUnit)
import System.Directory (renameFile)
import qualified System.Directory as D
import System.IO.Error (isDoesNotExistError)

class (Monad m) => PoiMonad m where
  moveFile :: FilePath -> FilePath -> m (Result ())
  listDirectory :: FilePath -> m (Result [FilePath])
  deleteFile :: FilePath -> m (Result ())
  displayMessages :: a -> m String
  searchFile :: FilePath -> String -> m (Maybe (Vector FilePath))
  doesFileExist :: FilePath -> m Bool

  getCurrentLocalDateTime :: m LocalTime
  getPastLocalDateTime :: Int -> TimeUnit -> m LocalTime

data PoiPure = PoiPure
  { poiPureDir :: !File
  , poiPureCurrentLocalDateTime :: !LocalTime
  , poiPureConsoleBuffer :: !(Maybe String)
  , poiPurePossibleException :: !(Maybe Error)
  }
  deriving (Show, Eq)

initialPoiEnv :: FilePath -> LocalTime -> PoiPure
initialPoiEnv p t =
  PoiPure
    (Directory p V.empty)
    t
    Nothing
    Nothing

evalPoiPure :: PoiPure -> a -> Result a
evalPoiPure (PoiPure{poiPurePossibleException}) v = case poiPurePossibleException of
  Just e -> Left e
  _anyOtherFailure -> Right v

instance PoiMonad (Reader PoiPure) where
  moveFile src dest = do
    env <- ask
    hasSrc <- doesFileExist src
    if not hasSrc then return $ Left (PoiIOError $ FileNotFound src)
      else
        return $ evalPoiPure env ()
  doesFileExist name = do
    env <- ask
    return $ F.doesFileExist (poiPureDir env) name

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
  doesFileExist = D.doesFileExist
