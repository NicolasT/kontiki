{-# LANGUAGE GADTs,
             RecordWildCards,
             OverloadedStrings,
             MultiWayIf #-}

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

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString.Builder as B

import Control.Monad.State.Class (get)

import Control.Lens

import Network.Kontiki.Monad
import Network.Kontiki.Types

-- | Utility to determine whether a set of votes forms a majority.
isMajority :: Monad m => Set NodeId -> TransitionT s m Bool
isMajority votes = do
    nodes <- view configNodes
    return $ Set.size votes >= Set.size nodes `div` 2 + 1

-- | Top-level handler for `SomeState' input states.
handle :: (Functor m, Monad m) => Config -> Event -> SomeState a -> m (SomeState a, [Command])
handle cfg evt state = case state of
    WrapState state'@Follower{} ->
        select `fmap` runTransitionT handleFollower cfg state' evt
    WrapState state'@Candidate{} ->
        select `fmap` runTransitionT handleCandidate cfg state' evt
    WrapState state'@Leader{} ->
        select `fmap` runTransitionT handleLeader cfg state' evt
  where
    select (a, _, c) = (a, c)

handleFollower :: (Functor m, Monad m) => Handler Follower a m
handleFollower evt =
    zoom followerLens $ case evt of
        EMessage sender msg -> case msg of
            MRequestVote m -> handleRequestVote sender m
            MRequestVoteResponse{} -> do
                logS "Received RequestVoteResponse message in Follower state, ignoring"
                ignore
            MHeartbeat m -> handleHeartbeat m
        ETimeout t -> handleTimeout t
  where
    followerLens = lens (\(Follower s) -> s) (\_ s -> Follower s)
    ignore = (wrap . Follower) `fmap` get

    handleRequestVote sender msg@RequestVote{..} = do
        currentTerm <- use fCurrentTerm

        if | rvTerm < currentTerm -> do
               logS "Received RequestVote for old term"
               send sender $ MRequestVoteResponse
                           $ RequestVoteResponse { rvrTerm = currentTerm
                                                 , rvrVoteGranted = False
                                                 }
               ignore
           | rvTerm > currentTerm -> do
               logS "Received RequestVote for newer term, bumping"

               fCurrentTerm .= rvTerm
               fVotedFor .= Nothing

               handleRequestVote2 sender msg
           | otherwise -> do
               logS "Recieved RequestVote for current term"
               handleRequestVote2 sender msg

    handleRequestVote2 sender RequestVote{..} = do
        votedFor <- use fVotedFor
        currentTerm <- use fCurrentTerm
        l <- use fLog

        if | (votedFor == Nothing || votedFor == Just rvCandidateId)
               && (rvLastLogIndex >= logLastIndex l && rvLastLogTerm >= logLastTerm l) -> do
               log [ B.byteString "Granting vote for term "
                   , logTerm rvTerm
                   , B.byteString " to "
                   , B.byteString sender
                   ]

               send sender $ MRequestVoteResponse
                           $ RequestVoteResponse { rvrTerm = currentTerm
                                                 , rvrVoteGranted = True
                                                 }

               resetElectionTimeout

               get >>= \s -> return $ wrap $ Follower s{ _fVotedFor = Just sender }
            | otherwise -> do
               log [ B.byteString "Rejecting vote for term "
                   , logTerm rvTerm
                   , B.byteString " to "
                   , B.byteString sender
                   ]
               send sender $ MRequestVoteResponse
                           $ RequestVoteResponse { rvrTerm = currentTerm
                                                 , rvrVoteGranted = False
                                                 }
               ignore

    handleHeartbeat (Heartbeat term) = do
        currentTerm <- use fCurrentTerm

        if | term == currentTerm -> do
               logS "Received heartbeat"
               resetElectionTimeout
               ignore
           | otherwise -> do
               logS "Ignoring heartbeat"
               ignore

    handleTimeout t = case t of
        ETElection -> becomeCandidate
        ETHeartbeat -> logS "Ignoring heartbeat timeout" >> ignore

    -- TODO Handle single-node case
    becomeCandidate = do
        currentTerm <- use fCurrentTerm
        let nextTerm = succTerm currentTerm

        log [ B.byteString "Becoming candidate for term "
            , logTerm nextTerm
            ]

        resetElectionTimeout

        nodeId <- view configNodeId
        l <- use fLog

        let state = CandidateState { _cCurrentTerm = nextTerm
                                   , _cVotes = Set.singleton nodeId
                                   , _cLog = l
                                   }

        let l = state ^. cLog
            t = state ^. cCurrentTerm

        broadcast $ MRequestVote
                  $ RequestVote { rvTerm = t
                                , rvCandidateId = nodeId
                                , rvLastLogIndex = logLastIndex l
                                , rvLastLogTerm = logLastTerm l
                                }

        return $ wrap $ Candidate state


-- | Handler for events when in `Candidate' state.
handleCandidate :: (Functor m, Monad m) => Handler Candidate a m
handleCandidate evt =
    zoom candidateLens $ case evt of
        EMessage sender msg -> case msg of
            MRequestVote m -> handleRequestVote sender m
            MRequestVoteResponse m -> handleRequestVoteResponse sender m
            MHeartbeat m -> handleHeartbeat sender m
        ETimeout t -> handleTimeout t
  where
    candidateLens = lens (\(Candidate s) -> s) (\_ s -> Candidate s)
    ignore = (wrap . Candidate) `fmap` get

    handleRequestVote sender RequestVote{..} = do
        currentTerm <- use cCurrentTerm

        if | rvTerm > currentTerm -> stepDown sender rvTerm
           | otherwise -> do
                 log [ B.byteString "Denying term "
                     , logTerm rvTerm
                     , B.byteString " to "
                     , B.byteString sender
                     ]
                 send sender $ MRequestVoteResponse
                             $ RequestVoteResponse { rvrTerm = currentTerm
                                                   , rvrVoteGranted = False
                                                   }
                 ignore

    handleRequestVoteResponse sender RequestVoteResponse{..} = do
        currentTerm <- use cCurrentTerm
        if | rvrTerm < currentTerm -> do
                 logS "Ignoring RequestVoteResponse for old term"
                 ignore
           | rvrTerm > currentTerm -> stepDown sender rvrTerm
           | not rvrVoteGranted -> do
                 logS "Ignoring RequestVoteResponse since vote wasn't granted"
                 ignore
           | otherwise -> do
                 logS "Received valid RequestVoteResponse"

                 votes <- Set.insert sender `fmap` use cVotes

                 m <- isMajority votes

                 if m
                     then becomeLeader
                     else get >>= \s -> return $ wrap $ Candidate $ s { _cVotes = votes }

    handleHeartbeat sender (Heartbeat term) = do
        currentTerm <- use cCurrentTerm
        if | term > currentTerm -> stepDown sender term
           | otherwise -> do
                 logS "Ignoring heartbeat"
                 ignore

    handleTimeout t = case t of
        ETElection -> do
            nodeId <- view configNodeId
            currentTerm <- use cCurrentTerm
            l <- use cLog

            logS "Election timeout occurred"

            let state = CandidateState { _cCurrentTerm = succTerm $ currentTerm
                                       , _cVotes = Set.singleton nodeId
                                       , _cLog = l
                                       }

            resetElectionTimeout

            broadcast $ MRequestVote
                      $ RequestVote { rvTerm = currentTerm
                                    , rvCandidateId = nodeId
                                    , rvLastLogIndex = logLastIndex l
                                    , rvLastLogTerm = logLastTerm l
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

        l <- use cLog

        return $ wrap $ Follower
                      $ FollowerState { _fCurrentTerm = term
                                      , _fVotedFor = Just sender
                                      , _fLog = l
                                      }

    becomeLeader = do
        currentTerm <- use cCurrentTerm
        l <- use cLog

        log [ B.byteString "Becoming leader for term"
            , logTerm $ currentTerm
            ]

        resetHeartbeatTimeout
        broadcast $ MHeartbeat
                  $ Heartbeat $ currentTerm

        return $ wrap $ Leader
                      $ LeaderState { _lCurrentTerm = currentTerm
                                    , _lLog = l
                                    }

-- | Handler for events when in `Leader' state.
handleLeader :: (Functor m, Monad m) => Handler Leader a m
handleLeader evt =
    zoom leaderLens $ case evt of
        EMessage sender msg -> case msg of
            MRequestVote m -> handleRequestVote sender m
            MRequestVoteResponse{} -> do
                -- TODO Stepdown if rvrTerm > current?
                logS "Ignoring RequestVoteResponse in leader state"
                ignore
            MHeartbeat m -> handleHeartbeat sender m
        ETimeout t -> handleTimeout t
  where
    leaderLens :: Lens' (Leader a) (LeaderState a)
    leaderLens = lens (\(Leader s) -> s) (\_ s -> Leader s)
    ignore = (wrap . Leader) `fmap` get

    handleRequestVote sender RequestVote{..} = do
        currentTerm <- use lCurrentTerm
        if | rvTerm > currentTerm -> stepDown sender rvTerm
           | otherwise -> logS "Ignore RequestVote for old term" >> ignore

    handleHeartbeat sender (Heartbeat term) = do
        currentTerm <- use lCurrentTerm
        if | term > currentTerm -> stepDown sender term
           | otherwise -> logS "Ignore heartbeat of old term" >> ignore

    handleTimeout t = case t of
        -- TODO Can this be ignored? Stepdown instead?
        ETElection -> logS "Ignore election timeout" >> ignore
        ETHeartbeat -> do
            logS "Sending heartbeats"

            resetHeartbeatTimeout

            currentTerm <- use lCurrentTerm

            broadcast $ MHeartbeat
                      $ Heartbeat currentTerm

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

        l <- use lLog

        return $ wrap $ Follower
                      $ FollowerState { _fCurrentTerm = term
                                      , _fVotedFor = Just sender
                                      , _fLog = l
                                      }


-- | The initial state and commands for a new node to start.
initialize :: Config -> (SomeState a, [Command])
initialize cfg = (wrap $ Follower state, commands)
  where
    state = FollowerState { _fCurrentTerm = Term 0
                          , _fVotedFor = Nothing
                          , _fLog = emptyLog
                          }
    commands = [CResetTimeout $ CTElection (cfg ^. configElectionTimeout, 2 * cfg ^. configElectionTimeout)]


-- | Get a `String' representation of the current state `Mode'.
stateName :: SomeState a -> String
stateName s = case s of
    WrapState Follower{} -> "Follower"
    WrapState Candidate{} -> "Candidate"
    WrapState Leader{} -> "Leader"

-- | Get the `Term' of `SomeState'.
stateTerm :: SomeState a -> Term
stateTerm s = case s of
    WrapState (Follower s') -> s' ^. fCurrentTerm
    WrapState (Candidate s') -> s' ^. cCurrentTerm
    WrapState (Leader s') -> s' ^. lCurrentTerm
