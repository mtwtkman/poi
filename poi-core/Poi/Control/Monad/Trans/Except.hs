{-# LANGUAGE KindSignatures #-}

module Poi.Control.Monad.Trans.Except where

import Data.Functor.Identity (Identity, runIdentity)
import Data.Kind (Type)
import Poi.Control.Monad.Free (Free (Impure, Pure))

newtype ExceptT e (m :: Type -> Type) a = ExceptT {runExceptT :: m (Either e a)}

instance (Functor m) => Functor (ExceptT e m) where
  fmap f = ExceptT . fmap (fmap f) . runExceptT

type Except e = Free (ExceptT e Identity)

runExcept :: Except e a -> Either e a
runExcept (Pure a) = return a
runExcept (Impure (ExceptT k)) = runIdentity k >>= runExcept
