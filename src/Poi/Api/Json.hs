{-# LANGUAGE DeriveGeneric #-}

module Poi.Api.Json where

import Data.Time (LocalTime)
import GHC.Generics

data Trash = Trash
  { name :: String
  , parentPath :: String
  , trashId :: String
  , trashedAt :: LocalTime
  }
  deriving (Generic, Show)

data IndexedTrash = IndexedTrash
  { index :: Int
  , trash :: Trash
  }
  deriving (Generic, Show)

data ListUp = ListUp
  { trashes :: [IndexedTrash]
  }
  deriving (Generic, Show)
