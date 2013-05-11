{-# LANGUAGE GADTs,
             RecordWildCards,
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

import Data.Set (Set)
import qualified Data.Set as Set

import qualified Data.ByteString.Builder as B

import Network.Kontiki.Monad
import Network.Kontiki.Types

-- | Utility to determine whether a set of votes forms a majority.
isMajority :: Set NodeId -> Transition Bool
isMajority votes = do
    cfg <- getConfig
    return $ Set.size votes >= Set.size (configNodes cfg) `div` 2 + 1

-- | Top-level handler for `SomeState' input states.
handle :: Config -> Event -> SomeState a -> (SomeState a, [Command])
handle cfg evt state = case state of
    WrapState state'@Follower{} ->
        runTransition handleFollower cfg state' evt
    WrapState state'@Candidate{} ->
        runTransition handleCandidate cfg state' evt
    WrapState state'@Leader{} ->
        runTransition handleLeader cfg state' evt

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
