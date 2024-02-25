module Poi.Type.File where

import Data.Vector (Vector)
import Poi.Type.Result (Error (PoiIOError), PoiIOError (FileNotFound), Result)

data File
  = File !FilePath
  | Directory !FilePath !(Vector File)
  deriving (Show, Eq)

doesFileExist :: File -> FilePath -> Bool
doesFileExist (File f) = (==) f
doesFileExist (Directory f _) = (==) f

checkFilePath :: (Monad m) => File -> FilePath -> m (Result ())
checkFilePath f path =
  return $
    if doesFileExist f path
      then Right ()
      else Left (PoiIOError $ FileNotFound path)
