module Test.Poi.Control.Monad.FreeTest (tests) where

import Poi.Control.Moand.Free (Free (..), liftF)
import System.Exit (exitSuccess)
import Test.Tasty (TestTree, testGroup)

data OpF a = GetOne !(String -> a) | PutOne !String !a | Exit

instance Functor OpF where
  fmap f (GetOne k) = GetOne (f . k)
  fmap f (PutOne s a) = PutOne s (f a)
  fmap _ Exit = Exit

type Op = Free OpF

putOne' :: String -> Op ()
putOne' s = liftF $ PutOne s ()

getOne' :: Op String
getOne' = liftF $ GetOne id

runOp :: Op r -> IO r
runOp (Pure r) = return r
runOp (Impure (GetOne f)) = getLine >>= runOp . f
runOp (Impure (PutOne s t)) = putStrLn s >> runOp t
runOp (Impure Exit) = exitSuccess

tests :: TestTree
tests = testGroup "Poi.Control.Free" []
