{-# LANGUAGE KindSignatures #-}

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

reader :: (r -> a) -> Reader r a
reader f = f <$> ask

runReader :: Reader r a -> r -> a
runReader (Pure a) _ = a
runReader (Impure k) r = runReader (runIdentity $ runReaderT k r) r
