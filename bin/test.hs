{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses #-}

module Main where

import Control.Monad.Identity

import Data.Binary (Binary, decode, encode)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Network.Kontiki.Raft

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
      testGroup "Serialization" [
          testGroup "Messages" [
              testProperty "Message Int" (prop_serialization :: Message Int -> Bool)
            , testProperty "RequestVote" (prop_serialization :: RequestVote -> Bool)
            , testProperty "RequestVoteResponse" (prop_serialization :: RequestVoteResponse -> Bool)
            , testProperty "AppendEntries Int" (prop_serialization :: AppendEntries Int -> Bool)
            , testProperty "AppendEntriesResponse" (prop_serialization :: AppendEntriesResponse -> Bool)
            ]
        , testGroup "State" [
              testProperty "SomeState" (prop_serialization :: SomeState -> Bool)
            , testProperty "Follower" (prop_serialization :: Follower -> Bool)
            , testProperty "Candidate" (prop_serialization :: Candidate -> Bool)
            , testProperty "Leader" (prop_serialization :: Leader -> Bool)
            ]
        , testProperty "Entry Int" (prop_serialization :: Entry Int -> Bool)
        ]
    , testProperty "handle" prop_handle
    ]

prop_serialization :: (Eq a, Binary a) => a -> Bool
prop_serialization a = decode (encode a) == a


-- A stub MonadLog which, well... has no log at all
newtype Stub a = Stub { unStub :: Identity a }
  deriving ( Functor
           , Monad
           )

instance MonadLog Stub Int where
    logEntry _ = return Nothing
    logLastEntry = return Nothing

-- Make sure there are no obvious 'error' cases in the FSM
prop_handle :: Config -> SomeState -> Event Int -> Bool
prop_handle cfg state event =
    let (state', commands) = runIdentity $ unStub $ handle cfg state event in
    state' `seq` commands `seq` True
