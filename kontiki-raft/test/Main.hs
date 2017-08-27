{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Kontiki.Raft.Classes.Test as K

import qualified Kontiki.Raft.Internal.AllServers.Test as KRIA
import qualified Kontiki.Raft.Test as KRT
import Kontiki.Raft.Types (Index, Term, RequestVoteRequest, RequestVoteResponse, AppendEntriesRequest, AppendEntriesResponse, VolatileState)

tests :: IO TestTree
tests = do
    krt <- KRT.tests
    kria <- KRIA.tests
    return $ testGroup "kontiki-raft" [
        K.tests @Index @Term @RequestVoteRequest @RequestVoteResponse @AppendEntriesRequest @AppendEntriesResponse @VolatileState
      , krt
      , kria
      ]

main :: IO ()
main = defaultMain =<< tests
