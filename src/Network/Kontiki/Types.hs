{-# LANGUAGE GADTs,
             StandaloneDeriving,
             KindSignatures,
             DataKinds,
             TemplateHaskell #-}
module Network.Kontiki.Types where

import Data.Word

import Data.Set (Set)

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.ByteString.Builder (Builder)

import Control.Lens hiding (Index)

-- | Representation of a `Term'.
newtype Term = Term Word64
  deriving (Show, Eq, Ord)

-- | Increment a `Term'.
succTerm :: Term -> Term
succTerm (Term t) = Term (succ t)

-- | Representation of an `Index'.
newtype Index = Index Word64
  deriving (Show, Eq, Ord)

-- | Type of node identifiers.
type NodeId = ByteString
-- | Type of `CandidateId' fields (as used in the paper).
type CandidateId = NodeId


-- | Representation of a log entry of command type `a'.
data Entry a = Entry { eTerm :: Term
                     , eIndex :: Index
                     , eCommand :: a
                     }
  deriving (Show, Eq)


-- | Representation of a `Log' of commands of type `a'.
newtype Log a = Log [Entry a]
  deriving (Show, Eq)

-- | An empty `Log'.
emptyLog :: Log a
emptyLog = Log []

-- | Retrieve the `Index' of the last `Entry' in a `Log', or a zero default
-- when empty.
logLastIndex :: Log a -> Index
logLastIndex (Log es) | null es = Index 0
                      | otherwise = eIndex $ head es

-- | Retrieve the `Term' of the last `Entry' in a `Log', or a zero default
-- when empty.
logLastTerm :: Log a -> Term
logLastTerm (Log es) | null es = Term 0
                     | otherwise = eTerm $ head es


-- | State maintained by a node in `Follower' mode.
data FollowerState a = FollowerState { fCurrentTerm :: Term
                                     , fVotedFor :: Maybe CandidateId
                                     , fLog :: Log a
                                     }
  deriving (Show, Eq)

-- | State maintained by a node in `Candidate' mode.
data CandidateState a = CandidateState { _cCurrentTerm :: Term
                                       , _cVotes :: Set NodeId
                                       , _cLog :: Log a
                                       }
  deriving (Show, Eq)

makeLenses ''CandidateState

-- | State maintained by a node in `Leader' mode.
data LeaderState a = LeaderState { _lCurrentTerm :: Term
                                 , _lLog :: Log a
                                 }
  deriving (Show, Eq)

makeLenses ''LeaderState

-- | Modes a node can be in.
data Mode = MFollower
          | MCandidate
          | MLeader
  deriving (Show, Eq)


-- | Type alias for a state in `MFollower' mode.
type Follower = State 'MFollower
-- | Type alias for a state in `MCandidate' mode.
type Candidate = State 'MCandidate
-- | Type alias for a state in `MLeader' mode.
type Leader = State 'MLeader

-- | Representation of a node state.
--
-- `s' represents the `Mode' the state is in.
-- `a' represents the type of commands maintained by the `Log'.
data State (s :: Mode) a where
    Follower :: FollowerState a -> Follower a
    Candidate :: CandidateState a -> Candidate a
    Leader :: LeaderState a -> Leader a

deriving instance Show a => Show (State s a)
deriving instance Eq a => Eq (State s a)


-- | Existential wrapper for `State'.
data SomeState a where
    WrapState :: State s a -> SomeState a

deriving instance Show a => Show (SomeState a)

instance Eq a => Eq (SomeState a) where
    WrapState s1@Follower{} == WrapState s2@Follower{} = s1 == s2
    WrapState s1@Candidate{} == WrapState s2@Candidate{} = s1 == s2
    WrapState s1@Leader{} == WrapState s2@Leader{} = s1 == s2
    WrapState _ == WrapState _ = False


wrap :: State s a -> SomeState a
wrap = WrapState


-- | Configuration
data Config = Config { _configNodeId :: NodeId
                     , _configNodes :: Set NodeId
                     , _configElectionTimeout :: Int
                     , _configHeartbeatTimeout :: Int
                     }
  deriving (Show, Eq)

makeLenses ''Config

-- | Representation of possible `ETimeout' event messages.
data ETimeout = ETElection
              | ETHeartbeat
  deriving (Show, Eq)

-- | Representation of possible `CTimeout' command messages.
data CTimeout = CTElection (Int, Int)
              | CTHeartbeat Int
  deriving (Show, Eq)


-- | Representation of incoming events.
data Event = EMessage NodeId Message
           | ETimeout ETimeout
  deriving (Show, Eq)

-- | Representation of outgoing commands.
data Command = CBroadcast Message
             | CSend NodeId Message
             | CResetTimeout CTimeout
             | CLog Builder
  deriving (Show)


-- | Representation of a `RequestVote' message.
data RequestVote = RequestVote { rvTerm :: Term
                               , rvCandidateId :: CandidateId
                               , rvLastLogIndex :: Index
                               , rvLastLogTerm :: Term
                               }
  deriving (Show, Eq)

-- | Representation of a `RequestVoteResponse' message.
data RequestVoteResponse = RequestVoteResponse { rvrTerm :: Term
                                               , rvrVoteGranted :: Bool
                                               }
  deriving (Show, Eq)

-- | Representation of a `Heartbeat' message.
--
-- Note this is not according to the paper, as long as `AcceptRequest'
-- messages are not implemented.
data Heartbeat = Heartbeat Term
  deriving (Show, Eq)

-- | Wrapper for all `Message' types.
data Message = MRequestVote RequestVote
             | MRequestVoteResponse RequestVoteResponse
             | MHeartbeat Heartbeat
  deriving (Show, Eq)


