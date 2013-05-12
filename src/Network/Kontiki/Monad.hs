{-# LANGUAGE GeneralizedNewtypeDeriving,
             FlexibleInstances,
             MultiParamTypeClasses,
             TypeFamilies #-}
module Network.Kontiki.Monad where

import Control.Applicative (Applicative)

import Data.Monoid

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B

import Control.Monad.RWS

import Control.Lens
import Control.Lens.Internal.Zoom

import Network.Kontiki.Types

newtype TransitionT s m r = T { unTransitionT :: RWST Config [Command] s m r }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader Config
           , MonadWriter [Command]
           , MonadState s
           , MonadRWS Config [Command] s
           )

instance Monad z => Zoom (TransitionT s z) (TransitionT t z) s t where
    zoom l t = T $ zoom l $ unTransitionT t

type instance Zoomed (TransitionT s m) = FocusingWith [Command] m


type Handler f a m = Event -> TransitionT (f a) m (SomeState a)

runTransitionT :: Handler f a m -> Config -> f a -> Event -> m (SomeState a, f a, [Command])
runTransitionT h c s e = runRWST (unTransitionT $ h e) c s

exec :: Monad m => Command -> TransitionT s m ()
exec c = tell [c]

resetElectionTimeout :: Monad m => TransitionT s m ()
resetElectionTimeout = do
    t <- view configElectionTimeout
    exec $ CResetTimeout
         $ CTElection (t, 2 * t)

resetHeartbeatTimeout :: Monad m => TransitionT s m ()
resetHeartbeatTimeout = do
    t <- view configHeartbeatTimeout
    exec $ CResetTimeout
         $ CTHeartbeat t

broadcast :: Monad m => Message -> TransitionT s m ()
broadcast = exec . CBroadcast

send :: Monad m => NodeId -> Message -> TransitionT s m ()
send n m = exec $ CSend n m

logS :: Monad m => ByteString -> TransitionT s m ()
logS = exec . CLog . B.byteString

log :: Monad m => [Builder] -> TransitionT s m ()
log = exec . CLog . mconcat

logTerm :: Term -> Builder
logTerm (Term t) = B.string8 $ show t
