module Poi.Control.Monad.Free where

data Free f a = Pure !a | Impure !(f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap f (Pure a) = Pure (f a)
  fmap f (Impure fa) = Impure (fmap (fmap f) fa)

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  Pure f <*> Pure a = Pure (f a)
  Pure f <*> Impure fa = Impure (fmap (fmap f) fa)
  Impure fa <*> f = Impure (fmap (<*> f) fa)

instance (Functor f) => Monad (Free f) where
  Pure f >>= k = k f
  Impure fa >>= k = Impure $ fmap (>>= k) fa

runFree :: (Monad m) => Free m a -> m a
runFree (Pure a) = return a
runFree (Impure fa) = fa >>= runFree

liftF :: (Functor f) => f a -> Free f a
liftF = Impure . fmap Pure
