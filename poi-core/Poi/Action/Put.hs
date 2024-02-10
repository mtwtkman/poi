module Poi.Action.Put where

import Poi.Entity.TrashCan (TrashCan)
import Poi.Type.Result (Result)
import Poi.Action (ActionT)
import Poi.Monad (PoiMonad)
import Control.Monad.Reader (MonadReader(ask))

put :: (PoiMonad m) => FilePath -> ActionT m ()
put f = do
  tc <- ask
  return ()
