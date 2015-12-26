module Network.Kontiki.RaftSpec where

import           Network.Kontiki.Raft
import           Test.Hspec

candidateSpec :: Spec
candidateSpec = describe "Raft Protocol" $ do

  it "steps down and grant vote on Request Vote if term is greater" $ do
    handleRequestVote
