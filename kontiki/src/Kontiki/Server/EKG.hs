{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Server.EKG (registerStats) where

import Control.Concurrent (MVar, readMVar)

import Control.Lens (view)

import Control.Monad.Logger (runNoLoggingT)

import Database.LevelDB.Base (DB)

import qualified Data.HashMap.Lazy as Map
import qualified System.Metrics as EKG

import qualified Kontiki.Raft.Classes.State.Persistent as K
import qualified Kontiki.Raft.Classes.State.Volatile as K
import qualified Kontiki.Raft as K

import Kontiki.State.Persistent (runPersistentStateT)
import Kontiki.State.Volatile (VolatileState)
import Kontiki.Types (Index(getIndex), Node(getNode), Term(getTerm))

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
