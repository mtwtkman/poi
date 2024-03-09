{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Poi.Arbitrary where

import Poi.Action (PoiPure (PoiPure))
import Poi.Type.Result (
  ActionError (DeleteError, ListUpError, RestoreError),
  Error,
  PoiIOError (FileNotFound, InvalidFilePath, SomethingWrong),
 )
import Test.QuickCheck (Arbitrary (arbitrary), oneof)
import Poi.Type.File (File (File))
import Test.QuickCheck.Instances ()

instance Arbitrary PoiIOError where
  arbitrary =
    oneof
      [ FileNotFound <$> arbitrary
      , InvalidFilePath <$> arbitrary
      , SomethingWrong <$> arbitrary
      ]

instance Arbitrary ActionError where
  arbitrary =
    oneof
      [ pure ListUpError
      , pure RestoreError
      , pure DeleteError
      ]

instance Arbitrary Error where
  arbitrary = arbitrary

instance Arbitrary File where
  arbitrary = File <$> arbitrary

instance Arbitrary PoiPure where
  arbitrary =
    PoiPure
      <$> arbitrary
      <*> arbitrary
      <*> arbitrary
      <*> arbitrary
