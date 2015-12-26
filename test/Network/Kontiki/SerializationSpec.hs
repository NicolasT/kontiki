module Network.Kontiki.SerializationSpec where

import           Data.Binary          (Binary, decode, encode)
import           Network.Kontiki.Raft
import           Test.Hspec
import           Test.QuickCheck

serializationSpec :: Spec
serializationSpec = do

  describe "Messages" $ do
    it "Message Int" $ property (prop_serialization :: Message Int -> Bool)
    it "RequestVote" $ property (prop_serialization :: RequestVote -> Bool)
    it "RequestVoteResponse" $ property (prop_serialization :: RequestVoteResponse -> Bool)
    it "AppendEntries Int" $ property (prop_serialization :: AppendEntries Int -> Bool)
    it "AppendEntriesResponse" $ property (prop_serialization :: AppendEntriesResponse -> Bool)

  describe "State" $ do
    it  "SomeState" $ property (prop_serialization :: SomeState -> Bool)
    it  "Follower" $ property (prop_serialization :: Follower -> Bool)
    it  "Candidate" $ property (prop_serialization :: Candidate -> Bool)
    it  "Leader" $ property (prop_serialization :: Leader -> Bool)

  describe "Entry Int" $ do
    it "Entry Int" $ property (prop_serialization :: Entry Int -> Bool)

prop_serialization :: (Eq a, Binary a) => a -> Bool
prop_serialization a = decode (encode a) == a
