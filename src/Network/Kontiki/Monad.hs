{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses,
             TypeFamilies,
             StandaloneDeriving #-}

module Network.Kontiki.Monad where

import Prelude hiding (log)

import Control.Applicative
import Control.Monad.RWS

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString)

import Control.Lens hiding (Index)

import Network.Kontiki.Log
import Network.Kontiki.Types

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

runTransitionT :: TransitionT a s m r -> Config -> s -> m (r, s, [Command a])
runTransitionT = runRWST . unTransitionT

broadcast :: (Monad m, IsMessage t a) => t -> TransitionT a f m ()
broadcast m = tell [CBroadcast $ toMessage m]

send :: (Monad m, IsMessage t a) => NodeId -> t -> TransitionT a f m ()
send n m = tell [CSend n (toMessage m)]

resetElectionTimeout :: Monad m => TransitionT a f m ()
resetElectionTimeout = do
    t <- view configElectionTimeout
    tell [CResetElectionTimeout t (2 * t)]

resetHeartbeatTimeout :: Monad m => TransitionT a f m ()
resetHeartbeatTimeout = do
    t <- view configHeartbeatTimeout
    tell [CResetHeartbeatTimeout t]

log :: Monad m => Builder -> TransitionT a f m ()
log b = tell [CLog b]

logS :: Monad m => ByteString -> TransitionT a f m ()
logS = log . byteString

resubmit :: Monad m => TransitionT a f m ()
resubmit = tell [CResubmit]

truncateLog :: Monad m => Index -> TransitionT a f m ()
truncateLog i = tell [CTruncateLog i]

logEntries :: Monad m => [Entry a] -> TransitionT a f m ()
logEntries es = tell [CLogEntries es]

type Handler a s m = Event a -> TransitionT a (InternalState s) m SomeState
type MessageHandler t a s m = NodeId -> t -> TransitionT a (InternalState s) m SomeState
type TimeoutHandler t a s m = TransitionT a (InternalState s) m SomeState
