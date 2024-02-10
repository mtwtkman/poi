module Poi.Control.Free where

data Free f a = Pure !a | Impure !(f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure fa) = Impure (fmap (fmap f) fa)

instance (Applicative f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Impure fa = Impure (fmap (fmap f) fa)
  Impure fa <*> f = Impure (fmap (<*> f) fa)
