{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Kontiki.Raft.Classes.Test as K

import Kontiki.Protocol.Types (AppendEntriesRequest, AppendEntriesResponse, RequestVoteRequest, RequestVoteResponse, Index, Term)
import Kontiki.State.Volatile (VolatileState)

import qualified Kontiki.Protocol.Node.Test as KPN
import qualified Kontiki.Protocol.Types.Test as KPT
import qualified Kontiki.State.Persistent.Test as KSP

tests :: IO TestTree
tests = do
    ksp <- KSP.tests
    return $ testGroup "kontiki" [
          KPN.tests
        , KPT.tests
        , ksp
        , K.tests @Index @Term @RequestVoteRequest @RequestVoteResponse @AppendEntriesRequest @AppendEntriesResponse @VolatileState
        ]

main :: IO ()
main = defaultMain =<< tests
