module Test.Poi.Control.Monad.Trans.ReaderTest (tests) where
import Test.Tasty (TestTree, testGroup)
import Poi.Control.Monad.Trans.Reader (Reader, ask)


calculateContentLen :: Reader String Int
calculateContentLen = do length <$> ask


tests :: TestTree
tests = testGroup "Reader Monad" []
