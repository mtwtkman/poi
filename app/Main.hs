module Main where

import Control.Monad
import Data.List
import Data.Semigroup ((<>))
import Options.Applicative
import Poi
import Poi.Entity
import Poi.Object

type PutOptions = [FilePath]

data Command = List | Put PutOptions deriving (Show, Eq)

putCommand :: Parser Command
putCommand = Put <$> many (argument str (metavar "FILEPATH"))

poiCommand :: Parser Command
poiCommand = subparser
  ( command "list" (info (pure List) (progDesc "List trahsed objects"))
  <> command "put" (info putCommand (progDesc "Move objects to trushbox safety"))
  )

runPoiCommand :: Command -> IO ()
runPoiCommand List = print "list command"
runPoiCommand (Put filepaths) = print $ intercalate "," filepaths

main :: IO ()
main = runPoiCommand =<< execParser (info poiCommand idm)
