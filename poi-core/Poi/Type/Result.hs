module Poi.Type.Result where

data PutError
  = FileNotFound
  deriving (Show, Eq)

data ActionError
  = PutError !PutError
  | ListUpError
  | RestoreError
  | DeleteError
  deriving (Show, Eq)

newtype Error
  = ActionError ActionError
  deriving (Show, Eq)

type Result a = Either Error a
