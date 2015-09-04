{-# LANGUAGE TemplateHaskell,
             GADTs,
             DataKinds,
             KindSignatures,
             StandaloneDeriving,
             TypeFamilies,
             RecordWildCards,
             DeriveGeneric,
             FlexibleInstances,
             ScopedTypeVariables,
             MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Types
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- Types that are commonly used through-out kontiki. Some of the most
-- important types are defined here, like `Index', `Term', states
-- for different modes, as well as events & commands 
-- (see `"Network.Kontiki.Types#events_commands"') and messages 
-- (see `"Network.Kontiki.Types#messages"').
-----------------------------------------------------------------------------
module Network.Kontiki.Types (
    
    -- * General types
      NodeId, NodeSet
    , Index(unIndex), index0, succIndex, prevIndex
    , Term, term0, succTerm
    , Entry(..)
    , Config(..), configNodeId, configNodes, configElectionTimeout, configHeartbeatTimeout
    
    -- * Node states
    , FollowerState(..), fCurrentTerm, fCommitIndex, fVotedFor
    , CandidateState(..), cCurrentTerm, cCommitIndex, cVotes
    , LeaderState(..), lCurrentTerm, lCommitIndex, lNextIndex, lLastIndex
    , Mode(..), mode
    , Follower, Candidate, Leader
    , State(..), SomeState(..), InternalState
    , Wrapable(wrap)
    
    -- * Events and Commands
    , Event(..)
    , ElectionTimeout
    , HeartbeatTimeout
    , Command(..)

    -- * Messages
    , Message(..)
    , IsMessage(..)
    , RequestVote(..)
    , RequestVoteResponse(..)
    , AppendEntries(..)
    , AppendEntriesResponse(..)
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word

import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as BS8

import Data.Binary (Binary(get, put))
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

import Control.Lens hiding (Index, elements)

import Test.QuickCheck (Arbitrary(arbitrary, shrink), Gen, elements, listOf, listOf1)

import GHC.Generics

-----------------------------------------------------------------------------
-- * General types
-----------------------------------------------------------------------------

-- | Identifier of a node.
type NodeId = ByteString

-- | Set of `NodeId's
type NodeSet = Set NodeId

-- | Index of log entries.
newtype Index = Index { unIndex :: Word64 }
  deriving (Show, Eq, Ord)

instance Binary Index where
    get = Index `fmap` B.getWord64le
    put = B.putWord64le . unIndex

instance Arbitrary Index where
    arbitrary = Index `fmap` arbitrary
    shrink = map Index . shrink . unIndex

-- | Starting `Index'.
index0 :: Index
index0 = Index 0

-- | Calculate the successor of a given `Index'.
succIndex :: Index -> Index
succIndex (Index i) = Index (succ i)

-- | Calculate the predecessor of a given `Index'.
prevIndex :: Index -> Index
prevIndex (Index i) = Index (if i == 0 then 0 else i - 1)

-- | Representation of a term.
newtype Term = Term Word64
  deriving (Show, Eq, Ord)

instance Binary Term where
    get = Term `fmap` B.getWord64le
    put (Term t) = B.putWord64le t

instance Arbitrary Term where
    arbitrary = Term `fmap` arbitrary
    shrink (Term t) = map Term $ shrink t

-- | Starting `Term'.
term0 :: Term
term0 = Term 0

-- | Calculate the successor of a given `Term'.
succTerm :: Term -> Term
succTerm (Term i) = Term (succ i)

-- | Entry as stored in a log.
data Entry a = Entry { eIndex :: Index  -- ^ `Index' of the entry
                     , eTerm :: Term    -- ^ `Term' of the entry
                     , eValue :: a      -- ^ Value contained in the entry
                     }
  deriving (Show, Eq)

instance Binary a => Binary (Entry a) where
    get = Entry <$> get <*> get <*> get
    put Entry{..} = put eIndex >> put eTerm >> put eValue

instance Arbitrary a => Arbitrary (Entry a) where
    arbitrary = Entry <$> arbitrary <*> arbitrary <*> arbitrary
    shrink Entry{..} = do
        i <- shrink eIndex
        t <- shrink eTerm
        v <- shrink eValue
        return $ Entry i t v

-- | Configuration of a cluster.
data Config = Config { 
    
    -- | `NodeId' of the running node
    _configNodeId           :: NodeId 
   
    -- | Set of `NodeId's of all nodes in the cluster       
  , _configNodes            :: NodeSet
 
    -- | Election timeout    
  , _configElectionTimeout  :: Int           
 
    -- | Heartbeat timeout
  , _configHeartbeatTimeout :: Int           
} deriving (Show, Eq)
makeLenses ''Config

instance Arbitrary Config where
    arbitrary = do
        n <- arbitraryBS
        ns <- listOf arbitraryBS
        e <- abs `fmap` arbitrary
        h <- abs `fmap` arbitrary
        return $ Config n (Set.fromList (n : ns)) e h

-- | Generate an arbitrary `ByteString'.
arbitraryBS :: Gen ByteString
arbitraryBS = BS8.pack `fmap` listOf1 arbitrary

-----------------------------------------------------------------------------
-- * Node states
--
-- The following types encapsulate the state (and provide generally useful
-- instances for the types) kept by nodes operating in different modes.
-----------------------------------------------------------------------------

-- | State kept when in `Follower' mode.
data FollowerState = FollowerState { _fCurrentTerm :: Term
                                   , _fCommitIndex :: Index
                                   , _fVotedFor    :: Maybe NodeId
                                   }
  deriving (Show, Eq, Generic)
makeLenses ''FollowerState

instance Binary FollowerState

instance Arbitrary FollowerState where
    arbitrary = do
        n <- arbitraryBS
        FollowerState <$> arbitrary <*> arbitrary <*> elements [Nothing, Just n]

-- | State kept when in `Candidate' mode.
data CandidateState = CandidateState { _cCurrentTerm :: Term
                                     , _cCommitIndex :: Index
                                     , _cVotes       :: NodeSet
                                     }
  deriving (Show, Eq, Generic)
makeLenses ''CandidateState

instance Binary CandidateState

instance Arbitrary CandidateState where
    arbitrary = do
        v <- Set.fromList `fmap` listOf1 arbitraryBS
        CandidateState <$> arbitrary <*> arbitrary <*> pure v

-- | State kept when in `Leader' mode.
data LeaderState = LeaderState { _lCurrentTerm :: Term
                               , _lCommitIndex :: Index
                               , _lNextIndex   :: Map NodeId Index
                               , _lLastIndex   :: Map NodeId Index
                               }
  deriving (Show, Eq, Generic)
makeLenses ''LeaderState

instance Binary LeaderState

instance Arbitrary LeaderState where
    arbitrary = LeaderState <$> arbitrary
                            <*> arbitrary
                            <*> (Map.fromList `fmap` (listOf1 $ (,) <$> arbitraryBS <*> arbitrary))
                            <*> (Map.fromList `fmap` (listOf1 $ (,) <$> arbitraryBS <*> arbitrary))
-- | Running modes.
data Mode = MFollower
          | MCandidate
          | MLeader
  deriving (Show, Eq)

-- * Utility type aliases for all running `Mode's.
type Follower = State 'MFollower
type Candidate = State 'MCandidate
type Leader = State 'MLeader

-- | State of a node.
data State (s :: Mode) where
    Follower :: FollowerState -> Follower
    Candidate :: CandidateState -> Candidate
    Leader :: LeaderState -> Leader

deriving instance Show (State s)
deriving instance Eq (State s)

instance Binary Follower where
    get = Follower `fmap` get
    put (Follower s) = put s

instance Arbitrary Follower where
    arbitrary = Follower `fmap` arbitrary
    shrink (Follower s) = map Follower $ shrink s

instance Binary Candidate where
    get = Candidate `fmap` get
    put (Candidate s) = put s

instance Arbitrary Candidate where
    arbitrary = Candidate `fmap` arbitrary
    shrink (Candidate s) = map Candidate $ shrink s

instance Binary Leader where
    get = Leader `fmap` get
    put (Leader s) = put s

instance Arbitrary Leader where
    arbitrary = Leader `fmap` arbitrary
    shrink (Leader s) = map Leader $ shrink s

-- | Existential wrapper for `State'.
--
-- This is what's exposed to public API.
data SomeState where
    WrapState :: State s -> SomeState

deriving instance Show SomeState

instance Eq SomeState where
    a == b = case a of
        WrapState (Follower s) -> case b of
            WrapState (Follower s') -> s == s'
            _ -> False
        WrapState (Candidate s) -> case b of
            WrapState (Candidate s') -> s == s'
            _ -> False
        WrapState (Leader s) -> case b of
            WrapState (Leader s') -> s == s'
            _ -> False

instance Binary SomeState where
    get = do
        t <- B.getWord8
        case t of
            1 -> WrapState `fmap` (get :: B.Get Follower)
            2 -> WrapState `fmap` (get :: B.Get Candidate)
            3 -> WrapState `fmap` (get :: B.Get Leader)
            _ -> fail "SomeState: invalid tag"

    put s = case s of
        WrapState (s'@Follower{}) -> B.putWord8 1 >> put s'
        WrapState (s'@Candidate{}) -> B.putWord8 2 >> put s'
        WrapState (s'@Leader{}) -> B.putWord8 3 >> put s'

instance Arbitrary SomeState where
    arbitrary = do
        (f :: Follower) <- arbitrary
        (c :: Candidate) <- arbitrary
        (l :: Leader) <- arbitrary
        elements [WrapState f, WrapState c, WrapState l]

    shrink s = case s of
        WrapState (s'@Follower{}) -> map WrapState $ shrink s'
        WrapState (s'@Candidate{}) -> map WrapState $ shrink s'
        WrapState (s'@Leader{}) -> map WrapState $ shrink s'

-- | Retrieve the `Mode' of `SomeState'.
mode :: SomeState -> Mode
mode s = case s of
    WrapState Follower{} -> MFollower
    WrapState Candidate{} -> MCandidate
    WrapState Leader{} -> MLeader

-- | Type family mapping a `State' to the corresponding internal state
-- type.
type family InternalState (s :: *) :: *
type instance InternalState Follower = FollowerState
type instance InternalState Candidate = CandidateState
type instance InternalState Leader = LeaderState

-- | A `Wrapable' is an internal state which can be wrapped into a `State'
-- (and as such into a `SomeState').
class Wrapable s where
    wrap :: s -> SomeState

instance Wrapable FollowerState where
    wrap s = WrapState $ Follower s

instance Wrapable CandidateState where
    wrap s = WrapState $ Candidate s

instance Wrapable LeaderState where
    wrap s = WrapState $ Leader s

-----------------------------------------------------------------------------
-- * Events and Commands
-----------------------------------------------------------------------------

-- | #events_commands# 
-- Representation of some incoming event.
data Event a = EMessage NodeId (Message a) -- ^ Incoming message from some node
             | EElectionTimeout            -- ^ Election timeout timer fired
             | EHeartbeatTimeout           -- ^ Heartbeat timeout timer fired
  deriving (Show, Eq)
  
instance Arbitrary a => Arbitrary (Event a) where
    arbitrary = do
        n <- arbitraryBS
        m <- arbitrary
        elements [EMessage n m, EElectionTimeout, EHeartbeatTimeout]
  
-- | Representation of an election timeout.
data ElectionTimeout = ElectionTimeout
  deriving (Show, Eq)
  
-- | Representation of a heartbeat timeout.
data HeartbeatTimeout = HeartbeatTimeout
  deriving (Show, Eq)

-- | Representation of a command to be executed as the result of a state
-- transition.
data Command a = CBroadcast (Message a)        -- ^ Broadcast a `Message' to all other nodes
               | CSend NodeId (Message a)      -- ^ Send a `Message' to some given node
               | CResetElectionTimeout Int Int -- ^ Reset the election timeout timer to some random value in the given interval
               | CResetHeartbeatTimeout Int    -- ^ Reset the heartbeat timeout timer to the given time
               | CLog Builder                  -- ^ Log a message
               | CTruncateLog Index            -- ^ Truncate the log to given `Index'
               | CLogEntries [Entry a]         -- ^ Append some entries to the log
               | CSetCommitIndex Index         -- ^ Set new commit `Index'

{-| 
  Manually created `Show' instance for `Command'.
  
  This instance has to be manually implemented
  to maintain compatibility with newer versions
  of "Data.ByteString.Builder" that no longer
  define an instance of `Show'.
-} 
instance Show a => Show (Command a) where
    showsPrec p c = showParen (p >= 11) $ case c of
        CBroadcast m -> showString "CBroadcast "
                      . showsPrec 11 m
        CSend n m -> showString "CSend "
                   . showsPrec 11 n
                   . showChar ' '
                   . showsPrec 11 m
        CResetElectionTimeout m n -> showString "CResetElectionTimeout "
                                   . showsPrec 11 m
                                   . showChar ' '
                                   . showsPrec 11 n
        CResetHeartbeatTimeout t -> showString "CResetHeartbeatTimeout "
                                  . showsPrec 11 t
        CLog b -> showString "CLog "
                . showsPrec 11 (toLazyByteString b)
        CTruncateLog i -> showString "CTruncateLog "
                        . showsPrec 11 i
        CLogEntries es -> showString "CLogEntries "
                        . showsPrec 11 es
        CSetCommitIndex i -> showString "CSetCommitIndex "
                        . showsPrec 11 i

instance Arbitrary a => Arbitrary (Command a) where
    arbitrary = do
        n <- arbitraryBS
        m <- arbitrary
        l1 <- arbitrary
        l2 <- arbitrary
        let l = min l1 l2
            l' = max l1 l2
        i <- arbitrary
        es <- listOf arbitrary
        elements [ CBroadcast m
                 , CSend n m
                 , CResetElectionTimeout l l'
                 , CResetHeartbeatTimeout l
                 , CLog $ byteString n
                 , CTruncateLog i
                 , CLogEntries es
                 ]

-----------------------------------------------------------------------------
-- * Messages
-----------------------------------------------------------------------------

-- | #messages# 
-- Enumeration of all message types.
data Message a = MRequestVote RequestVote                       -- ^ Wraps `RequestVote' 
               | MRequestVoteResponse RequestVoteResponse       -- ^ Wraps `RequestVoteResponse'
               | MAppendEntries (AppendEntries a)               -- ^ Wraps `AppendEntries'
               | MAppendEntriesResponse AppendEntriesResponse   -- ^ Wraps `AppendEntriesResponse'
  deriving (Show, Eq, Generic)

instance Binary a => Binary (Message a)

instance Arbitrary a => Arbitrary (Message a) where
    arbitrary = do
        m1 <- MRequestVote `fmap` arbitrary
        m2 <- MRequestVoteResponse `fmap` arbitrary
        m3 <- MAppendEntries `fmap` arbitrary
        m4 <- MAppendEntriesResponse `fmap` arbitrary
        elements [m1, m2, m3, m4]

-- | Class of types which can be wrapped into a `Message'.
class IsMessage t a where
    toMessage :: t -> Message a

instance IsMessage RequestVote a where toMessage = MRequestVote
instance IsMessage RequestVoteResponse a where toMessage = MRequestVoteResponse
instance (a0 ~ a1) => IsMessage (AppendEntries a0) a1 where toMessage = MAppendEntries
instance IsMessage AppendEntriesResponse a where toMessage = MAppendEntriesResponse

-- | Message broadcast to all nodes in the cluster by a node 
-- in `Candidate' mode in order to gather votes for it 
-- to transition to  a `Leader'.
-- See &#167; 5.2 of the paper.
data RequestVote = RequestVote {
    -- | `Candidate' 's `Term'
    rvTerm :: Term
 
    -- | `Candidate' requesting the vote
  , rvCandidateId :: NodeId
   
    -- | `Index' of `Candidate' 's last log entry (&#167; 5.4)  
  , rvLastLogIndex :: Index
 
    -- | `Term' of `Candidate' 's last log entry (&#167; 5.4)
  , rvLastLogTerm :: Term
} deriving (Show, Eq, Generic)

instance Binary RequestVote

instance Arbitrary RequestVote where
    arbitrary = RequestVote <$> arbitrary
                            <*> arbitraryBS
                            <*> arbitrary
                            <*> arbitrary

-- | Response to `RequestVote' message that indicates whether
-- recipient node acknowledges the leader.
--
-- See &#167; 5.1, 5.2, 5.4 of the paper.
data RequestVoteResponse = RequestVoteResponse { 
  
    -- | Current `term' for candidate to update itself,
    --   in case it is not the leader with highest `Term'
    rvrTerm         :: Term
  
    -- | `True' indicates `Candidate' received vote
  , rvrVoteGranted  :: Bool
} deriving (Show, Eq, Generic)

instance Binary RequestVoteResponse

instance Arbitrary RequestVoteResponse where
    arbitrary = RequestVoteResponse <$> arbitrary
                                    <*> arbitrary
-- | Sent by `Leader' to replicate log entries (&#167; 5.3)
--  
-- Also used as heartbeat (&#167; 5.2), which is indicated by
-- empty `aeEntries'.  
data AppendEntries a =  AppendEntries { 

    -- | `Leader' 's `Term'
    aeTerm          :: Term
      
    -- | `Leader' 's `NodeId'
  , aeLeaderId      :: NodeId
    
    -- | `Index' of log entry immediately preceding new ones
  , aePrevLogIndex  :: Index
    
    -- | `Term' of `aePrevLogIndex' entry
  , aePrevLogTerm   :: Term
    
    -- | Log entries to store (empty for heartbeat;
    --   the `Leader' may decide to send more 
    --   than one for efficiency)
  , aeEntries       :: [Entry a]
    
    -- | `Leader' 's commit index
  , aeCommitIndex   :: Index
} deriving (Show, Eq, Generic)

instance Binary a => Binary (AppendEntries a)

instance Arbitrary a => Arbitrary (AppendEntries a) where
    arbitrary = AppendEntries <$> arbitrary
                              <*> arbitraryBS
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary

-- | Response to `AppendEntries'.
--   
-- Also used by `Follower's for recovery
-- to indicate to `Leader' the missing events.
-- See &#167; 5.3. 
data AppendEntriesResponse = AppendEntriesResponse { 

    -- | currentTerm, for `Leader' to update itself 
    aerTerm         :: Term
    
    -- | true if `Follower' contained entry 
    --   matching `aePrevLogIndex' and `asPrevLogTerm'
  , aerSuccess      :: Bool
    
    -- | In case of `aerSuccess' being `False',
    --   the `Follower' may indicate to `Leader'
    --   what is it's last index, so that `Leader'
    --   can bulk-replay the missing entries
  , aerLastIndex    :: Index
} deriving (Show, Eq, Generic)

instance Binary AppendEntriesResponse

instance Arbitrary AppendEntriesResponse where
    arbitrary = AppendEntriesResponse <$> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary
