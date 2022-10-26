module Poi.EffMonad where

import qualified System.Directory as D
import Data.HashMap

class Monad m => FileMonad m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()

class Monad m => DirectoryMonad m where
  getCurrentDirectory :: m FilePath
  doesFileExist :: FilePath -> m Bool
  renameDirectory :: FilePath -> FilePath -> m ()
  removeDirectoryRecursive :: FilePath -> m ()

instance FileMonad IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile

instance DirectoryMonad IO where
  getCurrentDirectory = D.getCurrentDirectory
  doesFileExist = D.doesFileExist
  renameDirectory = D.renameDirectory
  removeDirectoryRecursive = D.removeDirectoryRecursive
