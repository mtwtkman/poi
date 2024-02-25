module Test.Poi.Control.Monad.Trans.ExceptTest (tests) where
import Test.Tasty (TestTree, testGroup)
import Poi.Control.Monad.Trans.Except (Except)


data TestError = Error1 | Error2

tests :: TestTree
tests = testGroup
  "Except Monad"
  []
