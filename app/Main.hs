module Main where

import Control.Monad
import Data.List
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Poi.Action.Setup as SetupOp
import Poi.Entity
import System.Directory
import System.FilePath.Posix

type PutOptions = [FilePath]

data Command = List | Put PutOptions | Setup deriving (Show, Eq)

putCommand :: Parser Command
putCommand = Put <$> many (argument str (metavar "FILEPATH"))

poiCommand :: Parser Command
poiCommand =
  subparser
    ( command "list" (info (pure List) (progDesc "List trahsed objects"))
        <> command "put" (info putCommand (progDesc "Move objects to trushbox safety"))
        <> command "setup" (info (pure Setup) (progDesc "Setup trashbox"))
    )

runPoiCommand :: Command -> IO ()
runPoiCommand List = print "list command"
runPoiCommand (Put filepaths) = print $ intercalate "," filepaths
runPoiCommand Setup = do
  home <- getHomeDirectory
  let trashboxDir = home </> ".poi"
  result <- SetupOp.createTrashBoxDirectory . MkTrashBox $ trashboxDir
  case result of
    SetupOp.CreatedTrahsBox -> print $ "created " ++ trashboxDir
    SetupOp.TrashBoxAlreadyExists -> print $ trashboxDir ++ " exists, so do nothing"

main :: IO ()
main = runPoiCommand =<< execParser (info poiCommand idm)
