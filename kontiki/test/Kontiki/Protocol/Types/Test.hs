{-# LANGUAGE TypeApplications #-}

module Kontiki.Protocol.Types.Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Kontiki.Protocol.Types (Index, Term)

import Kontiki.Test.Utils (prop_trip_message)

tests :: TestTree
tests = testGroup "Kontiki.Protocol.Types" [
      testGroup "Term" [
          testProperty "prop_trip_message" (prop_trip_message @Term)
        ]
    , testGroup "Index" [
          testProperty "prop_trip_message" (prop_trip_message @Index)
        ]
    ]
