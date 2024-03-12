{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}

module Poi.Action where

import Control.Exception (try)
import Data.Time (UTCTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Poi.Control.Monad.Trans.Reader (Reader, ask)
import Poi.Type.File (File (Directory), checkFilePath)
import qualified Poi.Type.File as F
import Poi.Type.Result (
  Error (PoiIOError),
  PoiIOError (FileNotFound, SomethingWrong),
  Result,
 )
import qualified System.Directory as D
import System.IO.Error (isDoesNotExistError)

class (Monad m) => PoiMonad m where
  moveFile :: FilePath -> FilePath -> m (Result ())
  listDirectory :: FilePath -> m (Result [FilePath])
  deleteFile :: FilePath -> m (Result ())
  displayMessages :: a -> m String
  searchFile :: FilePath -> String -> m (Maybe (Vector FilePath))
  doesFileExist :: FilePath -> m Bool

data PoiPure = PoiPure
  { poiPureDir :: !File
  , poiPureCurrentLocalDateTime :: !UTCTime
  , poiPureConsoleBuffer :: !(Maybe String)
  , poiPurePossibleException :: !(Maybe Error)
  }
  deriving (Show, Eq)

initialPoiEnv :: FilePath -> UTCTime -> PoiPure
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
    let f = poiPureDir env
    hasSrc <- checkFilePath f src
    hasDest <- checkFilePath f dest
    case (hasSrc, hasDest) of
      (Right (), Right ()) -> return $ evalPoiPure env ()
      (Left e, _) -> return $ Left e
      (_, Left e) -> return $ Left e
  doesFileExist name = do
    env <- ask
    return $ F.doesFileExist (poiPureDir env) name

tryRealIO :: FilePath -> IO a -> IO (Result a)
tryRealIO target action = do
  result <- try action
  case result of
    Right r -> return $ Right r
    Left e ->
      return $
        Left $
          PoiIOError
            ( if isDoesNotExistError e
                then FileNotFound target
                else SomethingWrong (show e)
            )

instance PoiMonad IO where
  moveFile src dest = tryRealIO src (D.renameFile src dest)
  doesFileExist = D.doesFileExist
  deleteFile name = tryRealIO name (D.removeFile name)
