module Poi.Control.Monad.Trans.Reader where

import Poi.Control.Monad.Free (Free (..))

type Reader r a = Free ((->) r) a

ask :: Reader r r
ask = Impure Pure
