{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Poi.Control.Monad.Trans.Reader where

import Data.Functor.Identity (Identity (runIdentity))
import Data.Kind (Type)
import Poi.Control.Monad.Free (Free (..))

newtype ReaderT r (m :: Type -> Type) a = ReaderT {runReaderT :: r -> m a}

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rf) = ReaderT $ \r -> fmap f (rf r)

type Reader r = Free (ReaderT r Identity)

ask :: Reader r r
ask = reader id

local :: (r' -> r) -> Reader r a -> Reader r' a
local f m = reader $ runReader m . f

reader :: (r -> a) -> Reader r a
reader f = Impure $ ReaderT (return . Pure . f)

runReader :: Reader r a -> r -> a
runReader (Pure a) _ = a
runReader (Impure k) r = runReader (runIdentity $ runReaderT k r) r
