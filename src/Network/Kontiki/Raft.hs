{-# LANGUAGE GADTs,
             RankNTypes #-}

module Network.Kontiki.Raft (
      module Network.Kontiki.Log
    , module Network.Kontiki.Types
    , module Network.Kontiki.Monad
    , handle
    , initialState
    , restore
    ) where

import qualified Data.Set as Set

import Control.Monad.Identity (runIdentity)

import Control.Lens

import Network.Kontiki.Log
import Network.Kontiki.Types
import Network.Kontiki.Monad
import Network.Kontiki.Raft.Utils (stepDown)

import qualified Network.Kontiki.Raft.Follower as Follower
import qualified Network.Kontiki.Raft.Candidate as Candidate
import qualified Network.Kontiki.Raft.Leader as Leader

handle :: (Functor m, Monad m, MonadLog m a) => Config -> SomeState -> Event a -> m (SomeState, [Command a])
handle config state event = case state of
    WrapState(Follower s') -> select `fmap` runTransitionT (Follower.handle event) config s'
    WrapState(Candidate s') -> select `fmap` runTransitionT (Candidate.handle event) config s'
    WrapState(Leader s') -> select `fmap` runTransitionT (Leader.handle event) config s'
  where
    select (a, _, c) = (a, c)

initialState :: SomeState
initialState = wrap $ FollowerState { _fCurrentTerm = term0
                                    , _fVotedFor = Nothing
                                    }

restore :: Config -> SomeState -> (SomeState, [Command a])
restore cfg s = case s of
    WrapState(Follower s') -> run $ s' ^. fCurrentTerm
    WrapState(Candidate s') -> run $ s' ^. cCurrentTerm
    WrapState(Leader s') -> run $ s' ^. lCurrentTerm
  where
    run :: forall a. Term -> (SomeState, [Command a])
    run t = fixup $ runIdentity $ runTransitionT (stepDown t) cfg s
    fixup (a, _, c) = (a, filter (not . isResubmit) c)
    isResubmit c = case c of
        CResubmit -> True
        _ -> False


singleNodeConfig :: NodeId -> Config
singleNodeConfig n = Config { _configNodeId = n
                            , _configNodes = Set.fromList [n]
                            , _configElectionTimeout = 10000
                            , _configHeartbeatTimeout = 5000
                            }
