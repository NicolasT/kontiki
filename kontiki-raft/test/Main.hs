{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Kontiki.Raft.Classes.Test as K

import qualified Kontiki.Raft.Test as KRT
import Kontiki.Raft.Types (Index, Term, RequestVoteRequest, RequestVoteResponse, AppendEntriesRequest, AppendEntriesResponse, VolatileState)

tests :: IO TestTree
tests = do
    krt <- KRT.tests
    return $ testGroup "kontiki-raft" [
        K.tests @Index @Term @RequestVoteRequest @RequestVoteResponse @AppendEntriesRequest @AppendEntriesResponse @VolatileState
      , krt
      ]

main :: IO ()
main = defaultMain =<< tests
