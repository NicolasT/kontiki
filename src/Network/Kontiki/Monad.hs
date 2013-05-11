{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.Kontiki.Monad where

import Data.Monoid

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B

import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Control.Monad.Reader as R
import Control.Monad.Writer (MonadWriter, Writer)
import qualified Control.Monad.Writer as W

import Network.Kontiki.Types

newtype Transition r = T { unTransition :: ReaderT Config (Writer [Command]) r }
  deriving ( Functor
           , Monad
           , MonadReader Config
           , MonadWriter [Command]
           )

type Handler f a = Event -> f a -> Transition (SomeState a)

runTransition :: Handler f a -> Config -> f a -> Event -> (SomeState a, [Command])
runTransition h c s e = W.runWriter $ flip R.runReaderT c $ unTransition $ h e s

getConfig :: Transition Config
getConfig = R.ask

getNodeId :: Transition NodeId
getNodeId = configNodeId `fmap` getConfig

exec :: Command -> Transition ()
exec c = W.tell [c]

resetElectionTimeout :: Transition ()
resetElectionTimeout = do
    cfg <- getConfig
    exec $ CResetTimeout
         $ CTElection (configElectionTimeout cfg, 2 * configElectionTimeout cfg)

resetHeartbeatTimeout :: Transition ()
resetHeartbeatTimeout = do
    cfg <- getConfig
    exec $ CResetTimeout
         $ CTHeartbeat $ configHeartbeatTimeout cfg

broadcast :: Message -> Transition ()
broadcast = exec . CBroadcast

send :: NodeId -> Message -> Transition ()
send n m = exec $ CSend n m

logS :: ByteString -> Transition ()
logS = exec . CLog . B.byteString

log :: [Builder] -> Transition ()
log = exec . CLog . mconcat

logTerm :: Term -> Builder
logTerm (Term t) = B.string8 $ show t
