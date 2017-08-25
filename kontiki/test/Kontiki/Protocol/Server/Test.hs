{-# LANGUAGE OverloadedStrings #-}

module Kontiki.Protocol.Server.Test (tests) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as T

import Proto3.Suite.Class (Message, fromByteString, toLazyByteString)

import Hedgehog (Gen, Property, forAll, property, tripping)
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Kontiki.Protocol.Server (RequestVoteRequest(RequestVoteRequest), RequestVoteResponse(RequestVoteResponse))

prop_trip :: (Message a, Eq a, Show a)
          => Gen a
          -> Property
prop_trip gen = property $ do
    msg <- forAll gen
    tripping msg toLazyByteString (fromByteString . B.toStrict)

tests :: TestTree
tests = testGroup "Kontiki.Protocol.Server" [
      testProperty "tripping RequestVoteRequest" (prop_trip genRequestVoteRequest)
    , testProperty "tripping RequestVoteResponse" (prop_trip genRequestVoteResponse)
    ]
  where
    genRequestVoteRequest = RequestVoteRequest <$> G.word64 R.constantBounded
                                               <*> (T.fromStrict <$> G.text (R.constant 1 20) G.unicode)
                                               <*> G.word64 R.constantBounded
                                               <*> G.word64 R.constantBounded
    genRequestVoteResponse = RequestVoteResponse <$> G.word64 R.constantBounded
                                                 <*> G.bool
