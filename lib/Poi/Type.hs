{-# LANGUAGE FlexibleInstances #-}
module Poi.Type where

class Serialize a where
  serialize :: a -> String

data DeserializeError = DeserializeFailecd

instance MonadFail (Either DeserializeError) where
  fail s = Left DeserializeFailecd

class Deserialize a where
  deserialize :: String -> Either DeserializeError a

