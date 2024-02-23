module Poi.Type.Result where

data PoiIOError
  = FileNotFound !FilePath
  | InvalidFilePath !FilePath
  | SomethingWrong !String
  deriving (Show, Eq)

data ActionError
  = ListUpError
  | RestoreError
  | DeleteError
  deriving (Show, Eq)

data Error
  = ActionError !ActionError
  | PoiIOError !PoiIOError
  deriving (Show, Eq)

type Result a = Either Error a
