{-# LANGUAGE FlexibleInstances #-}
module Poi.Type where

class Serialize a where
  serialize :: a -> String

data DeserializeError = DeserializeFailed deriving (Show, Eq)

type DeserializeResult a = Either DeserializeError a

instance MonadFail (Either DeserializeError) where
  fail s = Left DeserializeFailed

class Deserialize a where
  deserialize :: String -> DeserializeResult a

