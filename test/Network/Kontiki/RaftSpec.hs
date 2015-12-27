module Network.Kontiki.RaftSpec where

import           Data.Kontiki.MemLog
import           Network.Kontiki.Raft
import           Test.Hspec
import           Test.QuickCheck

raftSpec :: Spec
raftSpec = describe "Raft Protocol" $ do

  it "steps down and grant vote on Request Vote if term is greater" $ do
    handleRequestVote
