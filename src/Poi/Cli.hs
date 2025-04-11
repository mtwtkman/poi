module Poi.Cli (execPoiParser) where

import Options.Applicative (
  Alternative (some),
  Parser,
  ParserInfo,
  argument,
  auto,
  command,
  execParser,
  flag',
  fullDesc,
  header,
  help,
  helper,
  hsubparser,
  info,
  long,
  metavar,
  option,
  progDesc,
  short,
  str,
  (<**>),
  (<|>),
 )
import Poi.Action (PoiAction (..))

data BuryOption
  = BuryAll
  | BuryDayBefore Int
  | BuryIndex [Int]
  deriving (Show, Eq)

data PoiCommand
  = ListUpCommand
  | TossCommand [FilePath]
  | PickUpCommand [Int]
  | BuryCommand BuryOption
  | TuiCommand
  | VersionCommand
  deriving (Show, Eq)

listUpParser :: Parser PoiCommand
listUpParser = pure ListUpCommand

tossParser :: Parser PoiCommand
tossParser =
  TossCommand
    <$> some
      ( argument
          str
          ( metavar "TARGET ..."
              <> help "Files for trashing."
          )
      )

pickUpParser :: Parser PoiCommand
pickUpParser =
  PickUpCommand <$> some (argument auto (metavar "INDEX..."))

buryParser :: Parser PoiCommand
buryParser = BuryCommand <$> (empty' <|> dayBefore <|> index)
 where
  empty' :: Parser BuryOption
  empty' = flag' BuryAll (long "all" <> short 'a')

  dayBefore :: Parser BuryOption
  dayBefore = BuryDayBefore <$> option auto (long "day" <> short 'd' <> metavar "DAY")

  index :: Parser BuryOption
  index = BuryIndex <$> some (option auto (long "index" <> short 'i' <> metavar "INDEX"))

tuiParser :: Parser PoiCommand
tuiParser = pure TuiCommand

versionParser :: Parser PoiCommand
versionParser = pure VersionCommand

poiParser :: Parser PoiCommand
poiParser =
  hsubparser
    ( command "listup" (info listUpParser (progDesc "List up Poi's trash can contents"))
        <> command "toss" (info tossParser (progDesc "Move a target file to Poi's trash can"))
        <> command "pickup" (info pickUpParser (progDesc "Back a trashed file from Poi's trash can"))
        <> command "bury" (info buryParser (progDesc "Delete a target file permanently"))
        <> command "tui" (info tuiParser (progDesc "Start poi as tui application"))
        <> command "version" (info versionParser (progDesc "Show this version"))
    )

opts :: ParserInfo PoiCommand
opts = info (poiParser <**> helper) (fullDesc <> header "poi - Safety garbages management application")

execPoiParser :: IO PoiAction
execPoiParser = detectAction =<< execParser opts
 where
  detectAction :: PoiCommand -> IO PoiAction
  detectAction ListUpCommand = return ListUp
  detectAction (TossCommand p) = return (Toss p)
  detectAction (PickUpCommand i) = return (PickUpByIndex i)
  detectAction (BuryCommand BuryAll) = return EmptyTrashCan
  detectAction (BuryCommand (BuryDayBefore d)) = return $ DeleteDayBefore d
  detectAction (BuryCommand (BuryIndex i)) = return $ DeleteByIndex i
  detectAction TuiCommand = return StartTuiApplication
  detectAction VersionCommand = return ShowVersion
