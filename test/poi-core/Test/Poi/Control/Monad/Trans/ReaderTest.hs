module Test.Poi.Control.Monad.Trans.ReaderTest (tests) where

import Poi.Control.Monad.Trans.Reader (Reader, ask, local, runReader)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck (testProperty)

readerExample :: Reader String Int
readerExample = length <$> ask

localSpec :: String -> Reader String Int
localSpec s = local (<> s) readerExample

tests :: TestTree
tests =
  testGroup
    "Reader Monad"
    [ testProperty "runReader" $ \s -> runReader readerExample (s :: String) == length s
    , testProperty "local" $ \(s, addition) -> runReader (localSpec (addition :: String)) (s :: String) == length (addition <> s)
    ]
