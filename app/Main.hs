module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Poi.Action.Erase as EraseAction
import qualified Poi.Action.List as ListAction
import qualified Poi.Action.Move as MoveAction
import qualified Poi.Action.Prompt as PromptAction
import qualified Poi.Action.Setup as SetupAction
import Poi.Entity
import System.Directory
import System.Environment
import System.FilePath.Posix

type TrashOption = [FilePath]

type TrashBoxLocationOption = FilePath

data Command
  = List
  | Trash TrashOption
  | Back
  | Erase
  deriving (Show, Eq)

moveCommand :: Parser Command
moveCommand = Trash <$> many (argument str (metavar "FILEPATH"))

listCommand :: Parser Command
listCommand = pure List

backCommand :: Parser Command
backCommand = pure Back

eraseCommand :: Parser Command
eraseCommand = pure Erase

poiCommand :: Parser Command
poiCommand =
  subparser
    ( command "list" (info listCommand (progDesc "List trahsed objects"))
        <> command "trash" (info moveCommand (progDesc "Trash objects to trashbox safety"))
        <> command "back" (info backCommand (progDesc "Rollback a trashed object to its original location"))
        <> command "erase" (info eraseCommand (progDesc "Erase trashed object permanently"))
    )
    <**> helper

runPoiCommand :: TrashBox -> Command -> IO ()
runPoiCommand tb List = ListAction.printCurrentMetaInfoList tb
runPoiCommand tb (Trash filepaths) = mapM_ (MoveAction.trash tb) filepaths
runPoiCommand tb Back = do
  m <- PromptAction.askMetaInfo tb
  result <- MoveAction.back tb m
  case result of
    Right _ -> return ()
    Left reason -> print reason
runPoiCommand tb Erase = do
  m <- PromptAction.askMetaInfo tb
  EraseAction.erase tb m

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
