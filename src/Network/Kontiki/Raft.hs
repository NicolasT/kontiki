{-# LANGUAGE GADTs,
             StandaloneDeriving,
             KindSignatures,
             DataKinds,
             RecordWildCards,
             GeneralizedNewtypeDeriving,
             DeriveFunctor,
             OverloadedStrings #-}

module Network.Kontiki.Raft (
      Config(..)
    , initialize
    , NodeId
    , Message
    , CTimeout(..)
    , Command(..)
    , Event(..)
    , ETimeout(..)
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
-- TODO Add Binary instances for, well, about everything
-- TODO Add Arbitrary instances for, well, about everything

import Prelude hiding (log)

import Data.Word

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Monoid

import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.Writer (MonadWriter, Writer)
import qualified Control.Monad.Writer as W

import Data.ByteString (ByteString)
import Data.ByteString.Char8 ()
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B

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


wrap :: State s a -> SomeState a
wrap = WrapState


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


newtype Transition r = T { unTransition :: ReaderT Config (Writer [Command]) r }
  deriving ( Functor
           , Monad
           , MonadReader Config
           , MonadWriter [Command]
           )

type Handler f a = Event -> f a -> Transition (SomeState a)

runTransition :: Handler f a -> Config -> f a -> Event -> (SomeState a, [Command])
runTransition h c s e = W.runWriter $ flip R.runReaderT c $ unTransition $ h e s

getConfig :: Transition Config
getConfig = R.ask

getNodeId :: Transition NodeId
getNodeId = configNodeId `fmap` getConfig

-- | Utility to determine whether a set of votes forms a majority.
isMajority :: Set NodeId -> Transition Bool
isMajority votes = do
    cfg <- getConfig
    return $ Set.size votes >= Set.size (configNodes cfg) `div` 2 + 1

exec :: Command -> Transition ()
exec c = W.tell [c]

-- | Top-level handler for `SomeState' input states.
handle :: Config -> Event -> SomeState a -> (SomeState a, [Command])
handle cfg evt state = case state of
    WrapState state'@Follower{} ->
        runTransition handleFollower cfg state' evt
    WrapState state'@Candidate{} ->
        runTransition handleCandidate cfg state' evt
    WrapState state'@Leader{} ->
        runTransition handleLeader cfg state' evt

resetElectionTimeout :: Transition ()
resetElectionTimeout = do
    cfg <- getConfig
    exec $ CResetTimeout
         $ CTElection (configElectionTimeout cfg, 2 * configElectionTimeout cfg)

resetHeartbeatTimeout :: Transition ()
resetHeartbeatTimeout = do
    cfg <- getConfig
    exec $ CResetTimeout
         $ CTHeartbeat $ configHeartbeatTimeout cfg

broadcast :: Message -> Transition ()
broadcast = exec . CBroadcast

send :: NodeId -> Message -> Transition ()
send n m = exec $ CSend n m

logS :: ByteString -> Transition ()
logS = exec . CLog . B.byteString

log :: [Builder] -> Transition ()
log = exec . CLog . mconcat

logTerm :: Term -> Builder
logTerm (Term t) = B.string8 $ show t


handleFollower :: Handler Follower a
handleFollower evt state0@(Follower fs0) = case evt of
    EMessage sender msg -> case msg of
        MRequestVote m -> handleRequestVote sender m
        MRequestVoteResponse{} -> do
            logS "Received RequestVoteResponse message in Follower state, ignoring"
            ignore
        MHeartbeat m -> handleHeartbeat m
    ETimeout t -> handleTimeout t
  where
    ignore = return $ wrap state0

    handleRequestVote sender msg@RequestVote{..}
      | rvTerm < fCurrentTerm fs0 = do
          logS "Received RequestVote for old term"
          send sender $ MRequestVoteResponse
                      $ RequestVoteResponse { rvrTerm = fCurrentTerm fs0
                                            , rvrVoteGranted = False
                                            }
          ignore
      | rvTerm > fCurrentTerm fs0 = do
          logS "Received RequestVote for newer term, bumping"
          let fs = fs0 { fCurrentTerm = rvTerm
                       , fVotedFor = Nothing
                       }
          handleRequestVote2 sender msg fs
      | otherwise = do
          logS "Recieved RequestVote for current term"
          handleRequestVote2 sender msg fs0

    handleRequestVote2 sender RequestVote{..} fs@FollowerState{..}
      | (fVotedFor == Nothing || fVotedFor == Just rvCandidateId)
          && (rvLastLogIndex >= logLastIndex fLog && rvLastLogTerm >= logLastTerm fLog) = do
          log [ B.byteString "Granting vote for term "
              , logTerm rvTerm
              , B.byteString " to "
              , B.byteString sender
              ]

          send sender $ MRequestVoteResponse
                      $ RequestVoteResponse { rvrTerm = fCurrentTerm
                                            , rvrVoteGranted = True
                                            }

          resetElectionTimeout

          return $ wrap $ Follower fs{ fVotedFor = Just sender }
      | otherwise = do
          log [ B.byteString "Rejecting vote for term "
              , logTerm rvTerm
              , B.byteString " to "
              , B.byteString sender
              ]
          send sender $ MRequestVoteResponse
                      $ RequestVoteResponse { rvrTerm = fCurrentTerm
                                            , rvrVoteGranted = False
                                            }
          return $ wrap $ Follower fs

    handleHeartbeat (Heartbeat term)
      | term == fCurrentTerm fs0 = do
          logS "Received heartbeat"
          resetElectionTimeout
          ignore
      | otherwise = do
          logS "Ignoring heartbeat"
          ignore

    handleTimeout t = case t of
        ETElection -> becomeCandidate fs0
        ETHeartbeat -> logS "Ignoring heartbeat timeout" >> ignore

    -- TODO Handle single-node case
    becomeCandidate FollowerState{..} = do
        let nextTerm = succTerm fCurrentTerm

        log [ B.byteString "Becoming candidate for term "
            , logTerm nextTerm
            ]

        resetElectionTimeout

        nodeId <- getNodeId

        let state = CandidateState { cCurrentTerm = nextTerm
                                   , cVotes = Set.singleton nodeId
                                   , cLog = fLog
                                   }

        broadcast $ MRequestVote
                  $ RequestVote { rvTerm = cCurrentTerm state
                                , rvCandidateId = nodeId
                                , rvLastLogIndex = logLastIndex $ cLog state
                                , rvLastLogTerm = logLastTerm $ cLog state
                                }

        return $ wrap $ Candidate state


-- | Handler for events when in `Candidate' state.
handleCandidate :: Handler Candidate a
handleCandidate evt state0@(Candidate cs0) = case evt of
    EMessage sender msg -> case msg of
        MRequestVote m -> handleRequestVote sender m
        MRequestVoteResponse m -> handleRequestVoteResponse sender m
        MHeartbeat m -> handleHeartbeat sender m
    ETimeout t -> handleTimeout t
  where
    ignore = return $ wrap state0

    handleRequestVote sender RequestVote{..}
      | rvTerm > cCurrentTerm cs0 = stepDown sender rvTerm
      | otherwise = do
            log [ B.byteString "Denying term "
                , logTerm rvTerm
                , B.byteString " to "
                , B.byteString sender
                ]
            send sender $ MRequestVoteResponse
                        $ RequestVoteResponse { rvrTerm = cCurrentTerm cs0
                                              , rvrVoteGranted = False
                                              }
            ignore

    handleRequestVoteResponse sender RequestVoteResponse{..}
      | rvrTerm < cCurrentTerm cs0 = do
            logS "Ignoring RequestVoteResponse for old term"
            ignore
      | rvrTerm > cCurrentTerm cs0 = stepDown sender rvrTerm
      | not rvrVoteGranted = do
            logS "Ignoring RequestVoteResponse since vote wasn't granted"
            ignore
      | otherwise = do
            logS "Received valid RequestVoteResponse"

            let votes' = Set.insert sender $ cVotes cs0

            m <- isMajority votes'

            if m
                then becomeLeader
                else return $ wrap $ Candidate cs0 { cVotes = votes' }

    handleHeartbeat sender (Heartbeat term)
      | term > cCurrentTerm cs0 = stepDown sender term
      | otherwise = do
            logS "Ignoring heartbeat"
            ignore

    handleTimeout t = case t of
        ETElection -> do
            nodeId <- getNodeId

            logS "Election timeout occurred"

            let state = CandidateState { cCurrentTerm = succTerm $ cCurrentTerm cs0
                                       , cVotes = Set.singleton nodeId
                                       , cLog = cLog cs0
                                       }

            resetElectionTimeout
            broadcast $ MRequestVote
                      $ RequestVote { rvTerm = cCurrentTerm state
                                    , rvCandidateId = nodeId
                                    , rvLastLogIndex = logLastIndex $ cLog state
                                    , rvLastLogTerm = logLastTerm $ cLog state
                                    }

            return $ wrap $ Candidate state
        ETHeartbeat -> do
            logS "Ignoring heartbeat timer timeout"
            ignore

    stepDown sender term = do
        log [ B.byteString "Stepping down, received term "
            , logTerm term
            , B.byteString " from "
            , B.byteString sender
            ]

        send sender $ MRequestVoteResponse
                    $ RequestVoteResponse { rvrTerm = term
                                          , rvrVoteGranted = True
                                          }
        resetElectionTimeout

        return $ wrap $ Follower
                      $ FollowerState { fCurrentTerm = term
                                      , fVotedFor = Just sender
                                      , fLog = cLog cs0
                                      }

    becomeLeader = do
        log [ B.byteString "Becoming leader for term"
            , logTerm $ cCurrentTerm cs0
            ]

        resetHeartbeatTimeout
        broadcast $ MHeartbeat
                  $ Heartbeat $ cCurrentTerm cs0

        return $ wrap $ Leader
                      $ LeaderState { lCurrentTerm = cCurrentTerm cs0
                                    , lLog = cLog cs0
                                    }

-- | Handler for events when in `Leader' state.
handleLeader :: Handler Leader a
handleLeader evt state0@(Leader LeaderState{..}) = case evt of
    EMessage sender msg -> case msg of
        MRequestVote m -> handleRequestVote sender m
        MRequestVoteResponse{} -> do
            -- TODO Stepdown if rvrTerm > current?
            logS "Ignoring RequestVoteResponse in leader state"
            ignore
        MHeartbeat m -> handleHeartbeat sender m
    ETimeout t -> handleTimeout t
  where
    ignore = return $ wrap state0

    handleRequestVote sender RequestVote{..}
      | rvTerm > lCurrentTerm = stepDown sender rvTerm
      | otherwise = logS "Ignore RequestVote for old term" >> ignore

    handleHeartbeat sender (Heartbeat term)
      | term > lCurrentTerm = stepDown sender term
      | otherwise = logS "Ignore heartbeat of old term" >> ignore

    handleTimeout t = case t of
        -- TODO Can this be ignored? Stepdown instead?
        ETElection -> logS "Ignore election timeout" >> ignore
        ETHeartbeat -> do
            logS "Sending heartbeats"

            resetHeartbeatTimeout
            broadcast $ MHeartbeat
                      $ Heartbeat lCurrentTerm

            ignore

    stepDown sender term = do
        log [ B.byteString "Stepping down, received term "
            , logTerm term
            , B.byteString " from "
            , B.byteString sender
            ]

        send sender $ MRequestVoteResponse
                    $ RequestVoteResponse { rvrTerm = term
                                          , rvrVoteGranted = True
                                          }
        resetElectionTimeout

        return $ wrap $ Follower
                      $ FollowerState { fCurrentTerm = term
                                      , fVotedFor = Just sender
                                      , fLog = lLog
                                      }


-- | The initial state and commands for a new node to start.
initialize :: Config -> (SomeState a, [Command])
initialize Config{..} = (wrap $ Follower state, commands)
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
