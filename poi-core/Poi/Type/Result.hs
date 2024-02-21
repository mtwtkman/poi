module Poi.Type.Result where

data EffectfulError
  = FailedMovingFile !FilePath
  | InvalidFilePath !FilePath
  deriving (Show, Eq)

data PutError
  = FileNotFound
  deriving (Show, Eq)

data ActionError
  = PutError !PutError
  | ListUpError
  | RestoreError
  | DeleteError
  deriving (Show, Eq)

data Error
  = ActionError !ActionError
  | EffectfulError !EffectfulError
  deriving (Show, Eq)

type Result a = Either Error a
