module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Poi.Action.Move as MoveAction
import qualified Poi.Action.Setup as SetupAction
import qualified Poi.Action.List as ListAction
import Poi.Entity
import System.Directory
import System.Environment
import System.FilePath.Posix

type MoveOption = [FilePath]

type TrashBoxLocationOption = FilePath

data Command
  = List
  | Move MoveOption
  | Setup
  | Back
  | Delete
  deriving (Show, Eq)

moveCommand :: Parser Command
moveCommand = Move <$> many (argument str (metavar "FILEPATH"))

listCommand :: Parser Command
listCommand = pure List

setupCommand :: Parser Command
setupCommand = pure Setup

backCommand :: Parser Command
backCommand = pure Back

deleteCommand :: Parser Command
deleteCommand = pure Delete

poiCommand :: Parser Command
poiCommand =
  subparser
    ( command "list" (info listCommand (progDesc "List trahsed objects"))
        <> command "move" (info moveCommand (progDesc "Move objects to trashbox safety"))
        <> command "setup" (info setupCommand (progDesc "Setup trashbox"))
        <> command "back" (info backCommand (progDesc "Move back a trashed object to its original location"))
        <> command "delete" (info backCommand (progDesc "Delete trashed object from trashbox"))
    )
    <**> helper

runMoveCommand :: MoveOption -> IO ()
runMoveCommand (x : xs) = do
  runMoveCommand xs

runPoiCommand :: TrashBox -> Command -> IO ()
runPoiCommand tb List = ListAction.printCurrentIndexedMetaInfoList tb
runPoiCommand tb (Move filepaths) = mapM_ (MoveAction.trash tb) filepaths
runPoiCommand tb Setup = do
  result <- SetupAction.createTrashBoxDirectory tb
  case result of
    SetupAction.CreatedTrahsBox -> print $ "created " ++ show tb
    SetupAction.TrashBoxAlreadyExists -> print $ show tb ++ " exists, so do nothing"
runPoiCommand tb Back = do
  print $ "back, tb=" ++ show tb
runPoiCommand tb Delete = do
  print $ "delete, tb=" ++ show tb

doesNeedToSetup :: TrashBox -> IO Bool
doesNeedToSetup (MkTrashBox b) = doesPathExist b

trashBoxLocation :: IO TrashBox
trashBoxLocation = do
  poiRoot <- lookupEnv "POI_ROOT"
  MkTrashBox <$> maybe ((</> ".poi") <$> getHomeDirectory) absolutePath poiRoot

main :: IO ()
main = do
  tb <- trashBoxLocation
  tbExists <- doesNeedToSetup tb
  if tbExists
    then runPoiCommand tb =<< execParser (info poiCommand (progDesc "The default location of trashbox is `~/.poi` but you can specify the location of trashbox by using `POI_ROOT` environment variable."))
    else putStrLn $ show tb ++ "does not exists, so you need to run `poi setup`"
