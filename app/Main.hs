module Main where

import Control.Monad
import Data.List
import Data.Maybe
import Data.Semigroup ((<>))
import Options.Applicative
import Poi.Action.Move
import qualified Poi.Action.Setup as SetupOp
import Poi.Entity
import System.Directory
import System.Environment
import System.FilePath.Posix

type PutOption = [FilePath]

type TrashBoxLocationOption = FilePath

data Command
  = List
  | Put PutOption
  | Setup
  | Back
  deriving (Show, Eq)

putCommand :: Parser Command
putCommand = Put <$> many (argument str (metavar "FILEPATH"))

listCommand :: Parser Command
listCommand = pure List

setupCommand :: Parser Command
setupCommand = pure Setup

backCommand :: Parser Command
backCommand = pure Back

poiCommand :: Parser Command
poiCommand =
  subparser
    ( command "list" (info listCommand (progDesc "List trahsed objects"))
        <> command "put" (info putCommand (progDesc "Move objects to trushbox safety"))
        <> command "setup" (info setupCommand (progDesc "Setup trashbox"))
        <> command "back" (info backCommand (progDesc "Put back a trashed object to its original location"))
    )
    <**> helper

runPutCommand :: PutOption -> IO ()
runPutCommand (x : xs) = do
  runPutCommand xs

runPoiCommand :: TrashBox -> Command -> IO ()
runPoiCommand tb List = print $ "list command, tb=" ++ show tb
runPoiCommand tb (Put filepaths) = print $ intercalate "," filepaths ++ " tb=" ++ show tb
runPoiCommand tb Setup = do
  result <- SetupOp.createTrashBoxDirectory tb
  case result of
    SetupOp.CreatedTrahsBox -> print $ "created " ++ show tb
    SetupOp.TrashBoxAlreadyExists -> print $ show tb ++ " exists, so do nothing"
runPoiCommand tb Back = do
  print $ "back, tb=" ++ show tb

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
