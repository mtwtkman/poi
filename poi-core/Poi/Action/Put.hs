module Poi.Action.Put where

import Control.Monad.Reader (ReaderT)
import Poi.Entity.TrashCan (TrashCan)
import Poi.Type.Result (Result)

put :: TrashCan -> FilePath -> Result TrashCan
put t f = undefined
