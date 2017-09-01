{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.Protocol.Node.Test (tests) where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Kontiki.Protocol.Types (AppendEntriesRequest, AppendEntriesResponse, RequestVoteRequest, RequestVoteResponse)

import Kontiki.Test.Utils (prop_trip_message)

tests :: TestTree
tests = testGroup "Kontiki.Protocol.Node" [
      testProperty "tripping RequestVoteRequest" (prop_trip_message @RequestVoteRequest)
    , testProperty "tripping RequestVoteResponse" (prop_trip_message @RequestVoteResponse)
    , testProperty "tripping AppendEntriesRequest" (prop_trip_message @AppendEntriesRequest)
    , testProperty "tripping AppendEntriesResponse" (prop_trip_message @AppendEntriesResponse)
    ]
