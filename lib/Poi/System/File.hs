module Poi.System.File where

import System.Directory
import System.FilePath

absolutePath :: FilePath -> IO FilePath
absolutePath p
  | isAbsolute p = return p
  | otherwise = do
      d <- getCurrentDirectory
      return $ d </> p

