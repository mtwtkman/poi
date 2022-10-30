module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Poi.Action.List as ListAction
import qualified Poi.Action.Move as MoveAction
import qualified Poi.Action.Setup as SetupAction
import Poi.Entity
import System.Directory
import System.Environment
import System.FilePath.Posix

type MoveOption = [FilePath]

type TrashBoxLocationOption = FilePath

data Command
  = List
  | Move MoveOption
  | Back
  | Delete
  deriving (Show, Eq)

moveCommand :: Parser Command
moveCommand = Move <$> many (argument str (metavar "FILEPATH"))

listCommand :: Parser Command
listCommand = pure List

backCommand :: Parser Command
backCommand = pure Back

deleteCommand :: Parser Command
deleteCommand = pure Delete

poiCommand :: Parser Command
poiCommand =
  subparser
    ( command "list" (info listCommand (progDesc "List trahsed objects"))
        <> command "move" (info moveCommand (progDesc "Move objects to trashbox safety"))
        <> command "back" (info backCommand (progDesc "Move back a trashed object to its original location"))
        <> command "delete" (info backCommand (progDesc "Delete trashed object from trashbox"))
    )
    <**> helper

runPoiCommand :: TrashBox -> Command -> IO ()
runPoiCommand tb List = ListAction.printCurrentMetaInfoList tb
runPoiCommand tb (Move filepaths) = mapM_ (MoveAction.trash tb) filepaths
runPoiCommand tb Back = print $ "back, tb=" ++ show tb
runPoiCommand tb Delete = print $ "delete, tb=" ++ show tb

trashBoxLocation :: IO TrashBox
trashBoxLocation = do
  poiRoot <- lookupEnv "POI_ROOT"
  MkTrashBox <$> maybe ((</> ".poi") <$> getHomeDirectory) absolutePath poiRoot

main :: IO ()
main = do
  tb <- trashBoxLocation
  setupResult <- SetupAction.createTrashBoxDirectory tb
  when (setupResult == SetupAction.CreatedTrahsBox) (putStrLn $ "created " ++ show tb)
  runPoiCommand tb =<< execParser (info poiCommand (progDesc "The default location of trashbox is `~/.poi` but you can specify the location of trashbox by using `POI_ROOT` environment variable."))
