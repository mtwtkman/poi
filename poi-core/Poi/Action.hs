module Poi.Action where

import Control.Monad.Reader (ReaderT)
import Poi.Entity.TrashCan (TrashCan)

type ActionT m a = ReaderT TrashCan m a
