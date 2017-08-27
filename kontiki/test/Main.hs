{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Kontiki.Raft.Classes.Test as K

import Kontiki.Protocol.Server (AppendEntriesResponse, RequestVoteRequest, RequestVoteResponse)
import Kontiki.Protocol.Server.AppendEntriesRequest (AppendEntriesRequest)
import Kontiki.Protocol.Server.Instances ()
import Kontiki.State.Volatile (VolatileState)
import Kontiki.Types (Index, Term)

import qualified Kontiki.Protocol.Server.Test as KPS
import qualified Kontiki.State.Persistent.Test as KSP
import qualified Kontiki.Types.Test as KT

tests :: IO TestTree
tests = do
    ksp <- KSP.tests
    return $ testGroup "kontiki" [
          KPS.tests
        , ksp
        , KT.tests
        , K.tests @Index @Term @RequestVoteRequest @RequestVoteResponse @(AppendEntriesRequest Int) @AppendEntriesResponse @VolatileState
        ]

main :: IO ()
main = defaultMain =<< tests