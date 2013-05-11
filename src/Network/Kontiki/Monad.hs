{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Kontiki.Monad where

import Control.Applicative (Applicative)

import Data.Monoid

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B

import Control.Monad.RWS
import Control.Monad.Identity (Identity, runIdentity)

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

type Transition s r = TransitionT s Identity r

type Handler f a = Event -> f a -> Transition (f a) (SomeState a)

runTransition :: Handler f a -> Config -> f a -> Event -> (SomeState a, f a, [Command])
runTransition h c s e = runIdentity $ runRWST (unTransitionT $ h e s) c s

getConfig :: Transition s Config
getConfig = ask

getNodeId :: Transition s NodeId
getNodeId = configNodeId `fmap` getConfig

exec :: Command -> Transition s ()
exec c = tell [c]

resetElectionTimeout :: Transition s ()
resetElectionTimeout = do
    cfg <- getConfig
    exec $ CResetTimeout
         $ CTElection (configElectionTimeout cfg, 2 * configElectionTimeout cfg)

resetHeartbeatTimeout :: Transition s ()
resetHeartbeatTimeout = do
    cfg <- getConfig
    exec $ CResetTimeout
         $ CTHeartbeat $ configHeartbeatTimeout cfg

broadcast :: Message -> Transition s ()
broadcast = exec . CBroadcast

send :: NodeId -> Message -> Transition s ()
send n m = exec $ CSend n m

logS :: ByteString -> Transition s ()
logS = exec . CLog . B.byteString

log :: [Builder] -> Transition s ()
log = exec . CLog . mconcat

logTerm :: Term -> Builder
logTerm (Term t) = B.string8 $ show t
