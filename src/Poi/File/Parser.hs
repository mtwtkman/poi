module Poi.File.Parser (parseTrashedFilePath) where

import Data.Foldable (foldrM)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Data.Time (
  LocalTime,
  defaultTimeLocale,
  parseTimeM,
 )
import qualified Data.UUID as U
import Poi.Entity (
  Trash (Trash),
  TrashCanLocation,
  TrashedAtPath,
  buildTrashedAtPath,
  parentFileName,
  trashContainerName,
  trashedAtFormat,
 )
import Safe (atMay)
import System.Directory (listDirectory)
import System.FilePath (joinPath)
import Text.RE.Replace (Match (matchSource))
import Text.RE.TDFA.String (RE, compileRegex, (?=~))

pathRe :: IO RE
pathRe = compileRegex "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{12})?"

parseTrashedFilePath :: TrashCanLocation -> String -> IO [Trash]
parseTrashedFilePath can trashedAtString = do
  matchedResult <- pathRe <&> (?=~) trashedAtString
  t <- parseTimeM False defaultTimeLocale trashedAtFormat (matchSource matchedResult)
  let trashedAtPath = buildTrashedAtPath can t
  listDirectory trashedAtPath
    >>= foldrM
      (\fid acc -> makeTrash fid t trashedAtPath <&> (: acc))
      []
 where
  makeTrash :: String -> LocalTime -> TrashedAtPath -> IO Trash
  makeTrash fid t trashedAtPath =
    case U.fromString fid of
      Nothing -> error "invalid uuid"
      Just fid' -> do
        let container = joinPath [trashedAtPath, fid]
        p <- readFile $ joinPath [container, parentFileName]
        fs <- listDirectory $ joinPath [container, trashContainerName]
        let f = fromMaybe "" (atMay fs 0)
        return $ Trash f p fid' t
