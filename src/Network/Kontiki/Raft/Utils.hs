{-# LANGUAGE TypeFamilies #-}

module Network.Kontiki.Raft.Utils (
      handleGeneric
    ) where

import Control.Lens
import Control.Lens.Internal.Zoom (FocusingWith)

import Network.Kontiki.Types
import Network.Kontiki.Monad

type MessageHandler t f m a = NodeId -> t -> TransitionT (f a) m (SomeState a)
type TimeoutHandler f m a = TransitionT (f a) m (SomeState a)

-- TODO Simplify the first type madness
-- This should somehow be something as simple as "Lens' (s a) (f a)"
handleGeneric :: (Monad m, f ~ InternalState s)
              => ((f a -> FocusingWith [Command] m (SomeState a) (f a)) -> s a -> FocusingWith [Command] m (SomeState a) (s a))
              -> MessageHandler RequestVote f m a
              -> MessageHandler RequestVoteResponse f m a
              -> MessageHandler Heartbeat f m a
              -> TimeoutHandler f m a
              -> TimeoutHandler f m a
              -> Handler s a m
handleGeneric
    zoomLens
    handleRequestVote
    handleRequestVoteResponse
    handleHeartbeat
    handleElectionTimeout
    handleHeartbeatTimeout
    event = zoom zoomLens $ case event of
    EMessage sender msg -> case msg of
        MRequestVote m -> handleRequestVote sender m
        MRequestVoteResponse m -> handleRequestVoteResponse sender m
        MHeartbeat m -> handleHeartbeat sender m
    ETimeout t -> case t of
        ETElection -> handleElectionTimeout
        ETHeartbeat -> handleHeartbeatTimeout

