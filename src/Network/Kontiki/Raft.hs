{-# LANGUAGE GADTs,
             StandaloneDeriving,
             KindSignatures,
             DataKinds,
             RecordWildCards #-}

module Network.Kontiki.Raft (
      Config(..)
    , initialize
    , NodeId
    , Message
    , CTimeout(..)
    , Command(..)
    , Event(..)
    , ETimeout(..)
    , Step
    , Handler
    , handle
    , Log
    , emptyLog
    , SomeState
    , stateName
    , Term
    , stateTerm
    ) where

-- TODO Implement proper consensus-based lease extension
-- TODO Get rid of tons of code duplication
-- TODO Cleanup export list
-- TODO Wrap stuff into something like
--     Transition m r a = ReaderT Config (WriterT Command (StateT State m a) a) a
-- or whatever
-- TODO Given the above, add a 'CLog :: String -> Command' command to track things
-- TODO Add Binary instances for, well, about everything
-- TODO Add Arbitrary instances for, well, about everything

import Data.Word

import Data.Set (Set)
import qualified Data.Set as Set

import Data.ByteString (ByteString)

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
data CandidateState a = CandidateState { cCurrentTerm :: Term
                                       , cVotes :: Set NodeId
                                       , cLog :: Log a
                                       }
  deriving (Show, Eq)

-- | State maintained by a node in `Leader' mode.
data LeaderState a = LeaderState { lCurrentTerm :: Term
                                 , lLog :: Log a
                                 }
  deriving (Show, Eq)


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


-- | Utility to wrap the first `State' element of a tuple.
wrapFst :: (State s a, b) -> (SomeState a, b)
wrapFst (a, b) = (WrapState a, b)


-- | Configuration
data Config = Config { configNodeId :: NodeId
                     , configNodes :: Set NodeId
                     , configElectionTimeout :: Int
                     , configHeartbeatTimeout :: Int
                     }
  deriving (Show, Eq)


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
  deriving (Show, Eq)


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


-- | A `Step' takes an @f a@ (e.g. @Follower a@) into a @t a@ (e.g.
-- @Candidate a@) and a set of `Command's to execute.
type Step f t a = (f a) -> (t a, [Command])
-- | A `Handler' takes some `Config' and an `Event', then runs a `Step'
-- from `f' (e.g. `Follower') to `SomeState' given command type `a'.
type Handler f a = Config -> Event -> Step f SomeState a


-- | Top-level handler for `SomeState' input states.
handle :: Handler SomeState a
handle cfg evt state = case state of
    WrapState state'@Follower{} -> handleFollower cfg evt state'
    WrapState state'@Candidate{} -> handleCandidate cfg evt state'
    WrapState state'@Leader{} -> handleLeader cfg evt state'


-- | Handler for events when in `Follower' state.
handleFollower :: Handler Follower a
handleFollower Config{..} evt state@(Follower fs) = case evt of
    EMessage sender msg -> case msg of
        MRequestVote m -> handleRequestVote sender m
        MRequestVoteResponse{} -> ignore
        MHeartbeat m -> handleHeartbeat m
    ETimeout t -> handleTimeout t
  where
    ignore = wrapFst (state, [])

    requestVoteResponse s g = RequestVoteResponse { rvrTerm = fCurrentTerm s
                                                  , rvrVoteGranted = g
                                                  }

    bumpTerm s t = s { fCurrentTerm = t
                     , fVotedFor = Nothing
                     }

    handleRequestVote sender msg@RequestVote{..}
      | rvTerm < fCurrentTerm fs =
          wrapFst (state, [CSend sender $ MRequestVoteResponse $ requestVoteResponse fs False])
      | rvTerm > fCurrentTerm fs = handleRequestVote2 sender msg $ bumpTerm fs rvTerm
      | otherwise = handleRequestVote2 sender msg fs

    handleRequestVote2 sender RequestVote{..} fs'@FollowerState{..}
      | (fVotedFor == Nothing || fVotedFor == Just rvCandidateId)
          && (rvLastLogIndex >= logLastIndex fLog && rvLastLogTerm >= logLastTerm fLog) =
          wrapFst $ (Follower fs'{ fVotedFor = Just sender },
                     [ CSend sender $ MRequestVoteResponse $ requestVoteResponse fs' True
                     , CResetTimeout $ CTElection
                         (configElectionTimeout, 2 * configElectionTimeout)
                     ])
      | otherwise = wrapFst $
          (Follower fs',
           [CSend sender $ MRequestVoteResponse $ requestVoteResponse fs' False])

    handleHeartbeat (Heartbeat term)
      | term == fCurrentTerm fs = wrapFst $
          (state, [CResetTimeout $ CTElection (configElectionTimeout, 2 * configElectionTimeout)])
      | otherwise = ignore

    handleTimeout t = case t of
        ETElection -> wrapFst $ becomeCandidate fs
        ETHeartbeat -> ignore

    -- TODO Handle single-node case
    becomeCandidate FollowerState{..} = (Candidate state', commands)
      where
        state' = CandidateState { cCurrentTerm = succTerm fCurrentTerm
                                , cVotes = Set.singleton configNodeId
                                , cLog = fLog
                                }
        commands = [ CResetTimeout $ CTElection
                       (configElectionTimeout, 2 * configElectionTimeout)
                   , CBroadcast $ MRequestVote
                                $ RequestVote { rvTerm = cCurrentTerm state'
                                              , rvCandidateId = configNodeId
                                              , rvLastLogIndex = logLastIndex $ cLog state'
                                              , rvLastLogTerm = logLastTerm $ cLog state'
                                              }
                   ]

-- | Utility to determine whether a set of votes forms a majority.
isMajority :: Config -> Set NodeId -> Bool
isMajority Config{..} votes = Set.size votes >= Set.size configNodes `div` 2 + 1

-- | Handler for events when in `Candidate' state.
handleCandidate :: Handler Candidate a
handleCandidate cfg@Config{..} evt state@(Candidate cs) = case evt of
    EMessage sender msg -> case msg of
        MRequestVote m -> handleRequestVote sender m
        MRequestVoteResponse m -> handleRequestVoteResponse sender m
        MHeartbeat m -> handleHeartbeat sender m
    ETimeout t -> handleTimeout t
  where
    ignore = wrapFst (state, [])

    handleRequestVote sender RequestVote{..}
      | rvTerm > cCurrentTerm cs = wrapFst (stepDown sender rvTerm)
      | otherwise = wrapFst (state, [CSend sender $ MRequestVoteResponse
                                                  $ RequestVoteResponse { rvrTerm = cCurrentTerm cs
                                                                        , rvrVoteGranted = False
                                                                        }])

    handleRequestVoteResponse sender RequestVoteResponse{..}
      | rvrTerm < cCurrentTerm cs = ignore
      | rvrTerm > cCurrentTerm cs = wrapFst (stepDown sender rvrTerm)
      | not rvrVoteGranted = ignore
      | otherwise =
          let votes' = Set.insert sender $ cVotes cs in
          if isMajority cfg votes'
              then wrapFst becomeLeader
              else wrapFst $ (Candidate cs{ cVotes = votes' }, [])

    handleHeartbeat sender (Heartbeat term)
      | term > cCurrentTerm cs = wrapFst $ stepDown sender term
      | otherwise = ignore

    handleTimeout t = case t of
        ETElection ->
            let state' = CandidateState { cCurrentTerm = succTerm $ cCurrentTerm cs
                                        , cVotes = Set.singleton configNodeId
                                        , cLog = cLog cs
                                        } in
            let commands = [ CResetTimeout $ CTElection
                               (configElectionTimeout, 2 * configElectionTimeout)
                           , CBroadcast $ MRequestVote
                                        $ RequestVote { rvTerm = cCurrentTerm state'
                                                      , rvCandidateId = configNodeId
                                                      , rvLastLogIndex = logLastIndex $ cLog state'
                                                      , rvLastLogTerm = logLastTerm $ cLog state'
                                                      }
                           ] in
            wrapFst (Candidate state', commands)
        ETHeartbeat -> ignore

    stepDown sender term = (Follower fs, commands)
      where
        fs = FollowerState { fCurrentTerm = term
                           , fVotedFor = Just sender
                           , fLog = cLog cs
                           }
        commands = [ CSend sender $ MRequestVoteResponse $ RequestVoteResponse { rvrTerm = term
                                                                               , rvrVoteGranted = True
                                                                               }
                   , CResetTimeout $ CTElection
                       (configElectionTimeout, 2 * configElectionTimeout)
                   ]

    becomeLeader = (Leader ls, commands)
      where
        ls = LeaderState { lCurrentTerm = cCurrentTerm cs
                         , lLog = cLog cs
                         }
        commands = [ CResetTimeout $ CTHeartbeat configHeartbeatTimeout
                   , CBroadcast $ MHeartbeat $ Heartbeat $ lCurrentTerm ls
                   ]

-- | Handler for events when in `Leader' state.
handleLeader :: Handler Leader a
handleLeader Config{..} evt state@(Leader LeaderState{..}) = case evt of
    EMessage sender msg -> case msg of
        MRequestVote m -> handleRequestVote sender m
        MRequestVoteResponse{} -> ignore
        MHeartbeat m -> handleHeartbeat sender m
    ETimeout t -> handleTimeout t
  where
    ignore = wrapFst (state, [])

    handleRequestVote sender RequestVote{..}
      | rvTerm > lCurrentTerm = wrapFst (stepDown sender rvTerm)
      | otherwise = ignore

    handleHeartbeat sender (Heartbeat term)
      | term > lCurrentTerm = wrapFst (stepDown sender term)
      | otherwise = ignore

    handleTimeout t = case t of
        ETElection -> ignore
        ETHeartbeat -> wrapFst (state, commands)
      where
        commands = [ CResetTimeout $ CTHeartbeat configHeartbeatTimeout
                   , CBroadcast $ MHeartbeat $ Heartbeat lCurrentTerm
                   ]

    stepDown sender term = (Follower fs, commands)
      where
        fs = FollowerState { fCurrentTerm = term
                           , fVotedFor = Just sender
                           , fLog = lLog
                           }
        commands = [ CSend sender $ MRequestVoteResponse $ RequestVoteResponse { rvrTerm = term
                                                                               , rvrVoteGranted = True
                                                                               }
                   , CResetTimeout $ CTElection
                       (configElectionTimeout, 2 * configElectionTimeout)
                   ]

-- | The initial state and commands for a new node to start.
initialize :: Config -> (SomeState a, [Command])
initialize Config{..} = wrapFst (Follower state, commands)
  where
    state = FollowerState { fCurrentTerm = Term 0
                          , fVotedFor = Nothing
                          , fLog = emptyLog
                          }
    commands = [CResetTimeout $ CTElection (configElectionTimeout, 2 * configElectionTimeout)]

-- | Get a `String' representation of the current state `Mode'.
stateName :: SomeState a -> String
stateName s = case s of
    WrapState Follower{} -> "Follower"
    WrapState Candidate{} -> "Candidate"
    WrapState Leader{} -> "Leader"

-- | Get the `Term' of `SomeState'.
stateTerm :: SomeState a -> Term
stateTerm s = case s of
    WrapState (Follower FollowerState{..}) -> fCurrentTerm
    WrapState (Candidate CandidateState{..}) -> cCurrentTerm
    WrapState (Leader LeaderState{..}) -> lCurrentTerm
