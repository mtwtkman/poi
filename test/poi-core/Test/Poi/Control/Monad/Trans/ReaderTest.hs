module Test.Poi.Control.Monad.Trans.ReaderTest (tests) where

import Poi.Control.Monad.Trans.Reader (Reader, ask, runReader)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

calculateString :: Reader String Int
calculateString = length <$> ask

tests :: TestTree
tests =
  testGroup
    "Reader Monad"
    [ testProperty "runReader" $ \s -> runReader calculateString (s :: String) == (length s)
    ]
