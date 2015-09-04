{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses,
             TypeFamilies,
             StandaloneDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Kontiki.Monad
-- Copyright   :  (c) 2013, Nicolas Trangez
-- License     :  BSD-like
--
-- Maintainer  :  ikke@nicolast.be
--
-- This module introduces the main monad transformer `TransitionT' in 
-- which `Handler's, `MessageHandler's and `TimeoutHandler's live 
-- and defines actions that can be performed by them.
-----------------------------------------------------------------------------
module Network.Kontiki.Monad where

import Prelude hiding (log)

import Control.Monad.RWS

import Data.ByteString (ByteString)
import Data.ByteString.Lazy.Builder (Builder, byteString)

import Control.Lens hiding (Index)

import Network.Kontiki.Log
import Network.Kontiki.Types

-- | kontiki monad in which `Handler's, 
-- `MessageHandler's and `TimeoutHandler's live that adds the ability to
-- read `Config', issue `Command's and keep state `s' to an inner monad `m'.
newtype TransitionT a s m r = TransitionT { unTransitionT :: RWST Config [Command a] s m r }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Config
           , MonadWriter [Command a]
           , MonadState s
           , MonadRWS Config [Command a] s
           , MonadTrans
           )

instance (Monad m, MonadLog m a) => MonadLog (TransitionT a f m) a where
    logEntry = lift . logEntry
    logLastEntry = lift logLastEntry

-- | Runs `TransitionT'.
runTransitionT :: TransitionT a s m r -> Config -> s -> m (r, s, [Command a])
runTransitionT = runRWST . unTransitionT

-- | Broadcasts a message `m' to all nodes.
broadcast :: (Monad m, IsMessage t a) => t -> TransitionT a f m ()
broadcast m = tell [CBroadcast $ toMessage m]

-- | Sends a message `m' to a specific `NodeId' `n'. 
send :: (Monad m, IsMessage t a) => NodeId -> t -> TransitionT a f m ()
send n m = tell [CSend n (toMessage m)]

-- | Resets the election timeout.
resetElectionTimeout :: Monad m => TransitionT a f m ()
resetElectionTimeout = do
    t <- view configElectionTimeout
    tell [CResetElectionTimeout t (2 * t)]

-- | Resets the heartbeat timeout.
resetHeartbeatTimeout :: Monad m => TransitionT a f m ()
resetHeartbeatTimeout = do
    t <- view configHeartbeatTimeout
    tell [CResetHeartbeatTimeout t]

-- | Logs a message from this `Builder' `b'.
log :: Monad m => Builder -> TransitionT a f m ()
log b = tell [CLog b]

-- | Logs a message from this `ByteString'.
logS :: Monad m => ByteString -> TransitionT a f m ()
logS = log . byteString

-- | Truncates the log of events to `Index' `i'.
truncateLog :: Monad m => Index -> TransitionT a f m ()
truncateLog i = tell [CTruncateLog i]

-- | Adds entries `es' to the log.
logEntries :: Monad m => [Entry a] -> TransitionT a f m ()
logEntries es = tell [CLogEntries es]

-- | Sets new commit `Index' `i'
setCommitIndex :: Monad m => Index -> TransitionT a f m ()
setCommitIndex i = tell [CSetCommitIndex i]

-- | Handler of events.
type Handler a s m = 
                      Event a -- ^ `Event' to handle 
                   -> TransitionT a (InternalState s) m SomeState -- ^ new `TransitionT'

-- | Handler of messages.
type MessageHandler t a s m = 
                               NodeId -- ^ sender of the message 
                            -> t      -- ^ the mesage 
                            -> TransitionT a (InternalState s) m SomeState -- ^ new `TransitionT'
                            
-- | Handler of timeouts.                         
type TimeoutHandler t a s m = TransitionT a (InternalState s) m SomeState
