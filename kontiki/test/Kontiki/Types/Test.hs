{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.Types.Test (tests) where

import Data.Binary (Binary, encode, decode)

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Test.QuickCheck (Arbitrary)

import Hedgehog (Property, forAll, property, tripping)
import Hedgehog.Gen.QuickCheck (arbitrary)

import Kontiki.Types (Index, Term)

prop_binary :: forall a. (Eq a, Show a, Arbitrary a, Binary a) => Property
prop_binary = property $ forAll arbitrary >>= \(a :: a) -> tripping a encode (Just . decode)

tests :: TestTree
tests = testGroup "Kontiki.Types" [
      testGroup "Term" [
          testProperty "prop_binary" (prop_binary @Term)
        ]
    , testGroup "Index" [
          testProperty "prop_binary" (prop_binary @Index)
        ]
    ]
