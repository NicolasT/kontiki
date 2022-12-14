{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy (..))
import Kontiki (Index, NodeId (..), Term)
import qualified Kontiki.Raft as Raft
import qualified Kontiki.Raft.Class as Class
import Test.Hspec (hspec)

newtype Raft a = Raft {runRaft :: ReaderT (IORef (Maybe Term), IORef (Maybe (Maybe NodeId))) IO a}
  deriving (Functor)
  deriving newtype (Applicative, Monad, MonadIO)

instance Class.MonadRaftIO Raft where
  type MTerm Raft = Term
  type MIndex Raft = Index
  type MNodeId Raft = NodeId

  getCurrentTerm = Raft $ do
    (currentTerm, _) <- ask
    v <- liftIO $ readIORef currentTerm
    case v of
      Nothing -> error "No current term stored"
      Just v' -> return v'

  getVotedFor = Raft $ do
    (_, votedFor) <- ask
    v <- liftIO $ readIORef votedFor
    case v of
      Nothing -> error "No vote stored"
      Just v' -> return v'

  setPersistentState t n = Raft $ do
    (currentTerm, votedFor) <- ask
    liftIO $ do
      writeIORef currentTerm (Just t)
      writeIORef votedFor (Just n)

main :: IO ()
main = hspec $ do
  Class.tests (Proxy @Term) (Proxy @Index)
  Raft.tests $ \act -> do
    ct <- newIORef Nothing
    vf <- newIORef Nothing
    runReaderT (runRaft act) (ct, vf)
