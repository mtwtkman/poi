module Poi.Control.Monad.Trans.Reader where

import Poi.Control.Monad.Free (Free (..))

type ReaderF r = (->) r

type Reader r = Free (ReaderF r)

ask :: Reader r r
ask = Impure Pure

runReader :: Reader r a -> r -> a
runReader (Pure a) _ = a
runReader (Impure k) r = runReader (k r) r
