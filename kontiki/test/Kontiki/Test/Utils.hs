{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kontiki.Test.Utils (
      prop_trip_message
    ) where

import qualified Data.ByteString.Lazy as BS

import Proto3.Suite.Class (Message, fromByteString, toLazyByteString)

import Test.QuickCheck (Arbitrary)

import Hedgehog (Property, forAll, property, tripping)
import Hedgehog.Gen.QuickCheck (arbitrary)

prop_trip_message :: forall a. (Arbitrary a, Message a, Eq a, Show a) => Property
prop_trip_message = property $ do
    (msg :: a) <- forAll arbitrary
    tripping msg toLazyByteString (fromByteString . BS.toStrict)
