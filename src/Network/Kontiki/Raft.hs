{-# LANGUAGE GADTs #-}

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

import Control.Lens

import Network.Kontiki.Monad (runTransitionT)
import Network.Kontiki.Types

import qualified Network.Kontiki.Raft.Follower as Follower
import qualified Network.Kontiki.Raft.Candidate as Candidate
import qualified Network.Kontiki.Raft.Leader as Leader

-- | Top-level handler for `SomeState' input states.
handle :: (Functor m, Monad m) => Config -> Event -> SomeState a -> m (SomeState a, [Command])
handle cfg evt state = case state of
    WrapState state'@Follower{} ->
        select `fmap` runTransitionT (Follower.handle evt) cfg state'
    WrapState state'@Candidate{} ->
        select `fmap` runTransitionT (Candidate.handle evt) cfg state'
    WrapState state'@Leader{} ->
        select `fmap` runTransitionT (Leader.handle evt) cfg state'
  where
    select (a, _, c) = (a, c)


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
