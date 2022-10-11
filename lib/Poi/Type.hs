module Poi.Type where

class Serialize a where
  serialize :: a -> String

