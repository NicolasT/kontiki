{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- Note: We need to wrap the underlying AppendEntriesRequest for serdes of entries
module Kontiki.Protocol.Server.AppendEntriesRequest (
      AppendEntriesRequest(getAppendEntriesRequest)
    ) where

import Data.Typeable (Typeable)

import Data.Binary (Binary, encode, decode)

import qualified Data.ByteString.Lazy as BS

import qualified Data.Vector as V

import Control.Lens (lens)

import qualified Data.Text.Lazy as Text

import Test.QuickCheck (Arbitrary, arbitrary)
import Data.Text.Arbitrary ()

import qualified Kontiki.Raft.Classes.RPC as RPC
import qualified Kontiki.Raft.Classes.RPC.AppendEntriesRequest as AEReq

import Kontiki.Protocol.Server (Term(Term), Index(Index), NodeId(NodeId))
import qualified Kontiki.Protocol.Server as S

getTerm = S.termTerm
getIndex = S.indexIndex
getNode = S.nodeIdNode

newtype AppendEntriesRequest e = AppendEntriesRequest { getAppendEntriesRequest :: S.AppendEntriesRequest }
    deriving (Show, Eq, Typeable)

instance RPC.HasTerm (AppendEntriesRequest e) where
    type Term (AppendEntriesRequest e) = Term

    term = lens
        (Term . S.appendEntriesRequestTerm . getAppendEntriesRequest )
        (\(AppendEntriesRequest r) t -> AppendEntriesRequest r { S.appendEntriesRequestTerm = getTerm t })

instance Binary e => AEReq.AppendEntriesRequest (AppendEntriesRequest e) where
    type Node (AppendEntriesRequest e) = NodeId
    type Index (AppendEntriesRequest e) = Index
    type Entry (AppendEntriesRequest e) = e

    leaderId = lens
        (NodeId . Text.toStrict . S.appendEntriesRequestLeaderId . getAppendEntriesRequest)
        (\(AppendEntriesRequest r) n -> AppendEntriesRequest r { S.appendEntriesRequestLeaderId = Text.fromStrict $ getNode n })
    prevLogIndex = lens
        (Index . S.appendEntriesRequestPrevLogIndex . getAppendEntriesRequest)
        (\(AppendEntriesRequest r) i -> AppendEntriesRequest r { S.appendEntriesRequestPrevLogIndex = getIndex i })
    prevLogTerm = lens
        (Term . S.appendEntriesRequestPrevLogTerm . getAppendEntriesRequest)
        (\(AppendEntriesRequest r) t -> AppendEntriesRequest r { S.appendEntriesRequestPrevLogTerm = getTerm t })
    entries = lens
        (V.toList . V.map (decode . BS.fromStrict) . S.appendEntriesRequestEntries . getAppendEntriesRequest)
        (\(AppendEntriesRequest r) e -> AppendEntriesRequest r { S.appendEntriesRequestEntries = V.fromList $ map (BS.toStrict . encode) e })
    leaderCommit = lens
        (Index . S.appendEntriesRequestLeaderCommit . getAppendEntriesRequest)
        (\(AppendEntriesRequest r) c -> AppendEntriesRequest r { S.appendEntriesRequestLeaderCommit = getIndex c })

instance (Arbitrary e, Binary e) => Arbitrary (AppendEntriesRequest e) where
    arbitrary = do
        es <- arbitrary
        let es' = V.fromList $ map (BS.toStrict . encode) (es :: [e])
        req <- S.AppendEntriesRequest <$> arbitrary
                                      <*> (Text.fromStrict <$> arbitrary)
                                      <*> arbitrary
                                      <*> arbitrary
                                      <*> pure es'
                                      <*> arbitrary
        pure (AppendEntriesRequest req)
