module Kontiki.Raft.Internal.AllServers.Test (tests) where

import Control.Monad.Trans.State (runStateT)

import Data.Default (def)

import Control.Monad.Logger (runNoLoggingT)

import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, testSpec)

import Test.Hspec.Expectations (shouldBe)

import Kontiki.Raft (Role(Follower, Candidate), SomeState, initialState, role)
import Kontiki.Raft.Internal.AllServers (checkTerm)
import qualified Kontiki.Raft.Internal.State as S

import qualified Kontiki.Raft.Types as T

tests :: IO TestTree
tests = testSpec "Kontiki.Raft.Internal.AllServers" $ do
    describe "checkTerm" $ do
        it "updates currentTerm when applicable" $ do
            let p0 = T.PersistentState (T.Term 1) Nothing
                s0 = initialState :: SomeState T.VolatileState ()
                msg = T.RequestVoteRequest (T.Term 10) def def def

            (((), _), p) <- runNoLoggingT $ T.runPersistentStateT p0 $ flip runStateT s0 $ checkTerm msg
            T.persistentStateCurrentTerm p `shouldBe` T.Term 10

        it "discards old terms" $ do
            let p0 = T.PersistentState (T.Term 10) Nothing
                s0 = S.SomeState (S.C def) :: SomeState T.VolatileState ()
                msg = T.RequestVoteRequest (T.Term 5) def def def

            (((), s), p) <- runNoLoggingT $ T.runPersistentStateT p0 $ flip runStateT s0 $ checkTerm msg
            T.persistentStateCurrentTerm p `shouldBe` T.Term 10
            role s `shouldBe` Candidate

        it "downgrades to Follower" $ do
            let s0 = S.SomeState (S.C def) :: SomeState T.VolatileState ()
                p0 = T.PersistentState (T.Term 1) Nothing
                msg = T.RequestVoteRequest (T.Term 10) def def def

            role s0 `shouldBe` Candidate
            (((), s), _) <- runNoLoggingT $ T.runPersistentStateT p0 $ flip runStateT s0 $ checkTerm msg
            role s `shouldBe` Follower

