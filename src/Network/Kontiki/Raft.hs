{-# LANGUAGE GADTs,
             RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Raft
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- Model only implementation of the Raft protocol 
-- <https://ramcloud.stanford.edu/wiki/download/attachments/11370504/raft.pdf>.
--
-- This module provides the main API for using the model implementation of
-- the Raft protocol. Importing this module should be sufficient to implement
-- a driver to the `Network.Kontiki.Types.Command's emitted from the model. 
-----------------------------------------------------------------------------
module Network.Kontiki.Raft (
      module Network.Kontiki.Log
    , module Network.Kontiki.Types
    , module Network.Kontiki.Monad
    , handle
    , initialState
    , restore
    ) where

import Control.Lens

import Network.Kontiki.Log
import Network.Kontiki.Types
import Network.Kontiki.Monad

import qualified Network.Kontiki.Raft.Follower as Follower
import qualified Network.Kontiki.Raft.Candidate as Candidate
import qualified Network.Kontiki.Raft.Leader as Leader

-- | Main handler function which, given the `config' of the cluster 
-- and `state' of the node, runs the Raft protocol and  
-- returns the new state of the node and a list of commands that 
-- should be executed by the driver.
handle :: (Monad m, MonadLog m a) 
       => Config                        -- ^ configuration of the cluster 
       -> SomeState                     -- ^ current state of the node
       -> Event a                       -- ^ incoming event
       -> m (SomeState, [Command a])    -- ^ new state and list of commands
handle config state event = case state of
    WrapState(Follower s') -> select `fmap` runTransitionT (Follower.handle event) config s'
    WrapState(Candidate s') -> select `fmap` runTransitionT (Candidate.handle event) config s'
    WrapState(Leader s') -> select `fmap` runTransitionT (Leader.handle event) config s'
  where
    -- | Drops the middle value from a three-tuple
    select :: (a, b, c) -> (a, c)
    select (a, _, c) = (a, c)

-- | Initial state of all nodes.
initialState :: SomeState
initialState = wrap FollowerState { _fCurrentTerm = term0
                                  , _fCommitIndex = index0
                                  , _fVotedFor = Nothing
                                  , _fLastKnownLeader = Nothing
                                  }

-- | Restores the node to initial (`Follower') mode
-- and resets the election timeout. This function is useful
-- in order to kickstart the protocol by issuing the
-- `CResetElectionTimeout' command.
--
-- If the node is in either `Leader' or `Candidate' mode,
-- their state will be changed to having voted for itself.
--
-- In all modes, the current term will be kept.
restore :: Config                   -- ^ configuration of the cluster 
        -> SomeState                -- ^ current state of the node 
        -> (SomeState, [Command a]) -- ^ new state and list of commands
restore cfg s = case s of
    WrapState(Follower _) -> (s, commands)
    WrapState(Candidate s') -> (toFollower (s' ^. cCurrentTerm) (s' ^. cCommitIndex), commands)
    WrapState(Leader s') -> (toFollower (s' ^. lCurrentTerm) (s' ^. lCommitIndex), commands)
  where
    toFollower t i = wrap FollowerState { _fCurrentTerm = t
                                        , _fCommitIndex = i
                                        , _fVotedFor = Just nodeId
                                        , _fLastKnownLeader = Nothing
                                        }
    nodeId = cfg ^. configNodeId
    et = cfg ^. configElectionTimeout
    commands :: forall a. [Command a]
    commands = [CResetElectionTimeout et (2 * et)]
