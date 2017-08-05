{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Kontiki.Raft.Follower (
      convertToFollower
    , onRequestVoteRequest
    ) where

import Control.Monad.State.Class (MonadState, modify)

import Control.Monad.Indexed.State (IxMonadState, imodify)

import Control.Lens ((&), (.~), (^.))

import Data.Default.Class (Default(def))

import Kontiki.Raft.Classes.State.Volatile (VolatileState(commitIndex, lastApplied))

import Kontiki.Raft.State (Role(Follower), State(F), SomeState(SomeState))

convertToFollower :: ( MonadState (SomeState volatileState volatileLeaderState) m
                     , VolatileState volatileState
                     , Default volatileState
                     )
                  => m ()
convertToFollower = modify $ \s -> case s of
    SomeState s' -> SomeState $ F $ def & commitIndex .~ s' ^. commitIndex
                                        & lastApplied .~ s' ^. lastApplied

onRequestVoteRequest :: (IxMonadState m
                        )
                     => a
                     -> m (State vs vls 'Follower) (SomeState vs vls) ()
onRequestVoteRequest _ = do
    imodify SomeState
