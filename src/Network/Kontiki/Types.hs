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

module Network.Kontiki.Types (
      NodeId
    , Index(unIndex), index0, succIndex, prevIndex
    , Term, term0, succTerm
    , Entry(..)
    , Config(..), configNodeId, configNodes, configElectionTimeout, configHeartbeatTimeout
    , Follower, Candidate, Leader
    , State(..), SomeState(..), InternalState
    , Wrapable(wrap)
    , FollowerState(..), fCurrentTerm, fVotedFor
    , CandidateState(..), cCurrentTerm, cVotes
    , LeaderState(..), lCurrentTerm, lNextIndex, lLastIndex
    , Command(..)
    , Event(..)
    , Message(..)
    , IsMessage(..)
    , RequestVote(..)
    , RequestVoteResponse(..)
    , AppendEntries(..)
    , AppendEntriesResponse(..)
    , ElectionTimeout
    , HeartbeatTimeout
    ) where

import Control.Applicative

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)
import qualified Data.ByteString.Char8 as BS8

import Data.Binary (Binary(get, put))
import qualified Data.Binary.Get as B
import qualified Data.Binary.Put as B

import Control.Lens hiding (Index, elements)

import Test.QuickCheck (Arbitrary(arbitrary, shrink), Gen, elements, listOf, listOf1)

import GHC.Generics

type NodeId = ByteString

newtype Index = Index { unIndex :: Word64 }
  deriving (Show, Eq, Ord)

instance Binary Index where
    get = Index `fmap` B.getWord64le
    put = B.putWord64le . unIndex

instance Arbitrary Index where
    arbitrary = Index `fmap` arbitrary
    shrink = map Index . shrink . unIndex

index0 :: Index
index0 = Index 0

succIndex :: Index -> Index
succIndex (Index i) = Index (succ i)

prevIndex :: Index -> Index
prevIndex (Index i) = Index (if i == 0 then 0 else i - 1)

newtype Term = Term Word64
  deriving (Show, Eq, Ord)

instance Binary Term where
    get = Term `fmap` B.getWord64le
    put (Term t) = B.putWord64le t

instance Arbitrary Term where
    arbitrary = Term `fmap` arbitrary
    shrink (Term t) = map Term $ shrink t

term0 :: Term
term0 = Term 0

succTerm :: Term -> Term
succTerm (Term i) = Term (succ i)

data Entry a = Entry { eIndex :: Index
                     , eTerm :: Term
                     , eValue :: a
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

data Config = Config { _configNodeId :: NodeId
                     , _configNodes :: Set NodeId
                     , _configElectionTimeout :: Int
                     , _configHeartbeatTimeout :: Int
                     }
  deriving (Show, Eq)
makeLenses ''Config

instance Arbitrary Config where
    arbitrary = do
        n <- arbitraryBS
        ns <- listOf arbitraryBS
        e <- abs `fmap` arbitrary
        h <- abs `fmap` arbitrary
        return $ Config n (Set.fromList (n : ns)) e h


arbitraryBS :: Gen ByteString
arbitraryBS = BS8.pack `fmap` listOf1 arbitrary


data FollowerState = FollowerState { _fCurrentTerm :: Term
                                   , _fVotedFor :: Maybe NodeId
                                   }
  deriving (Show, Eq, Generic)
makeLenses ''FollowerState

instance Binary FollowerState

instance Arbitrary FollowerState where
    arbitrary = do
        n <- arbitraryBS
        FollowerState <$> arbitrary <*> elements [Nothing, Just n]


data CandidateState = CandidateState { _cCurrentTerm :: Term
                                     , _cVotes :: Set NodeId
                                     }
  deriving (Show, Eq, Generic)
makeLenses ''CandidateState

instance Binary CandidateState

instance Arbitrary CandidateState where
    arbitrary = do
        v <- Set.fromList `fmap` listOf1 arbitraryBS
        CandidateState <$> arbitrary <*> pure v


data LeaderState = LeaderState { _lCurrentTerm :: Term
                               , _lNextIndex :: Map NodeId Index
                               , _lLastIndex :: Map NodeId Index
                               }
  deriving (Show, Eq, Generic)
makeLenses ''LeaderState

instance Binary LeaderState

instance Arbitrary LeaderState where
    arbitrary = LeaderState <$> arbitrary
                            <*> (Map.fromList `fmap` (listOf1 $ (,) <$> arbitraryBS <*> arbitrary))
                            <*> (Map.fromList `fmap` (listOf1 $ (,) <$> arbitraryBS <*> arbitrary))


data Mode = MFollower
          | MCandidate
          | MLeader
  deriving (Show, Eq)

type Follower = State 'MFollower
type Candidate = State 'MCandidate
type Leader = State 'MLeader

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

type family InternalState (s :: *) :: *
type instance InternalState Follower = FollowerState
type instance InternalState Candidate = CandidateState
type instance InternalState Leader = LeaderState

class Wrapable s where
    wrap :: s -> SomeState

instance Wrapable FollowerState where
    wrap s = WrapState $ Follower s

instance Wrapable CandidateState where
    wrap s = WrapState $ Candidate s

instance Wrapable LeaderState where
    wrap s = WrapState $ Leader s


data ElectionTimeout = ElectionTimeout
  deriving (Show, Eq)
data HeartbeatTimeout = HeartbeatTimeout
  deriving (Show, Eq)

data Event a = EMessage NodeId (Message a)
             | EElectionTimeout
             | EHeartbeatTimeout
  deriving (Show, Eq)

instance Arbitrary a => Arbitrary (Event a) where
    arbitrary = do
        n <- arbitraryBS
        m <- arbitrary
        elements [EMessage n m, EElectionTimeout, EHeartbeatTimeout]

data Command a = CBroadcast (Message a)
               | CSend NodeId (Message a)
               | CResetElectionTimeout Int Int
               | CResetHeartbeatTimeout Int
               | CLog Builder
               | CResubmit
               | CTruncateLog Index
               | CLogEntries [Entry a]
  deriving (Show)

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
                 , CResubmit
                 , CTruncateLog i
                 , CLogEntries es
                 ]

data RequestVote = RequestVote { rvTerm :: Term
                               , rvCandidateId :: NodeId
                               , rvLastLogIndex :: Index
                               , rvLastLogTerm :: Term
                               }
  deriving (Show, Eq, Generic)

instance Binary RequestVote

instance Arbitrary RequestVote where
    arbitrary = RequestVote <$> arbitrary
                            <*> arbitraryBS
                            <*> arbitrary
                            <*> arbitrary

data RequestVoteResponse = RequestVoteResponse { rvrTerm :: Term
                                               , rvrVoteGranted :: Bool
                                               }
  deriving (Show, Eq, Generic)

instance Binary RequestVoteResponse

instance Arbitrary RequestVoteResponse where
    arbitrary = RequestVoteResponse <$> arbitrary
                                    <*> arbitrary


data AppendEntries a = AppendEntries { aeTerm :: Term
                                     , aeLeaderId :: NodeId
                                     , aePrevLogIndex :: Index
                                     , aePrevLogTerm :: Term
                                     , aeEntries :: [Entry a]
                                     , aeCommitIndex :: Index
                                     }
  deriving (Show, Eq, Generic)

instance Binary a => Binary (AppendEntries a)

instance Arbitrary a => Arbitrary (AppendEntries a) where
    arbitrary = AppendEntries <$> arbitrary
                              <*> arbitraryBS
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary
                              <*> arbitrary


data AppendEntriesResponse = AppendEntriesResponse { aerTerm :: Term
                                                   , aerSuccess :: Bool
                                                   , aerLastIndex :: Index
                                                   }
  deriving (Show, Eq, Generic)

instance Binary AppendEntriesResponse

instance Arbitrary AppendEntriesResponse where
    arbitrary = AppendEntriesResponse <$> arbitrary
                                      <*> arbitrary
                                      <*> arbitrary

data Message a = MRequestVote RequestVote
               | MRequestVoteResponse RequestVoteResponse
               | MAppendEntries (AppendEntries a)
               | MAppendEntriesResponse AppendEntriesResponse
  deriving (Show, Eq, Generic)

instance Binary a => Binary (Message a)

instance Arbitrary a => Arbitrary (Message a) where
    arbitrary = do
        m1 <- MRequestVote `fmap` arbitrary
        m2 <- MRequestVoteResponse `fmap` arbitrary
        m3 <- MAppendEntries `fmap` arbitrary
        m4 <- MAppendEntriesResponse `fmap` arbitrary
        elements [m1, m2, m3, m4]

class IsMessage t a where
    toMessage :: t -> Message a

instance IsMessage RequestVote a where toMessage = MRequestVote
instance IsMessage RequestVoteResponse a where toMessage = MRequestVoteResponse
instance (a0 ~ a1) => IsMessage (AppendEntries a0) a1 where toMessage = MAppendEntries
instance IsMessage AppendEntriesResponse a where toMessage = MAppendEntriesResponse
