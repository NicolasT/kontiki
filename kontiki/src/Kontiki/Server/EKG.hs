{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Server.EKG (
      forkServerWith
    , registerStats
    ) where

import Control.Concurrent (killThread, threadDelay)
import Control.Concurrent.MVar (MVar, readMVar)

import Control.Lens (view)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (runNoLoggingT)

import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Catch (MonadMask)

import Data.Monoid ((<>))

import Database.LevelDB.Base (DB)

import Control.Exception.Safe (bracket)

import qualified Data.HashMap.Lazy as Map
import qualified System.Metrics as EKG
import qualified System.Remote.Monitoring as EKG

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS8

import Control.Concurrent.Async.Lifted.Safe (Async, Forall, Pure)

import Katip (KatipContext, Severity(InfoS), logTM, ls)

import qualified Kontiki.Raft.Classes.State.Persistent as K
import qualified Kontiki.Raft.Classes.State.Volatile as K
import qualified Kontiki.Raft as K

import Kontiki.Protocol.Types (Index(getIndex), Node(getNode), Term(getTerm))
import Kontiki.Server.Async (withAsync)
import Kontiki.State.Persistent (runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)

registerStats :: DB -> MVar (K.SomeState VolatileState ()) -> EKG.Store -> IO ()
registerStats db state store = do
    let dbStats = Map.fromList [ ("kontiki.node.votedFor", EKG.Label . maybe "" getNode . fst)
                               , ("kontiki.node.currentTerm", EKG.Gauge . fromIntegral . getTerm . snd)
                               ]
        dbAct = runNoLoggingT $ runPersistentStateT db $
            (,) <$> K.getVotedFor <*> K.getCurrentTerm
    EKG.registerGroup dbStats dbAct store

    let mapRole = \case
            K.Follower -> "follower"
            K.Candidate -> "candidate"
            K.Leader -> "leader"
        stateStats = Map.fromList [ ("kontiki.node.role", EKG.Label . mapRole . K.role)
                                  , ("kontiki.node.commitIndex", EKG.Gauge . fromIntegral . getIndex . view K.commitIndex . K.volatileState)
                                  , ("kontiki.node.lastApplied", EKG.Gauge . fromIntegral . getIndex . view K.lastApplied . K.volatileState)
                                  ]
    EKG.registerGroup stateStats (readMVar state) store


forkServerWith :: ( MonadIO m
                  , MonadBaseControl IO m
                  , MonadMask m
                  , Forall (Pure m)
                  , KatipContext m
                  )
               => EKG.Store
               -> ByteString
               -> Int
               -> (Async a -> m b)
               -> m b
forkServerWith store host port = withAsync "ekg" act
  where
    act = bracket
        (liftIO $ EKG.forkServerWith store host port)
        (liftIO . killThread . EKG.serverThreadId)
        (const $ do
            $(logTM) InfoS $ ls $ "EKG is now running on http://" <> BS8.unpack host <> ":" <> show port
            sleepForever)
    sleepForever = do
        liftIO $ threadDelay (60 * 1000 * 1000)
        sleepForever
