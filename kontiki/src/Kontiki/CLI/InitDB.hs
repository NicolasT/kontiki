{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Kontiki.CLI.InitDB (main) where

import Data.Monoid ((<>))

import System.IO (stderr)

import qualified Database.LevelDB.Base as DB

import qualified Data.Text.Lazy as Text

import Control.Exception.Safe (bracket, catchAny, displayException)

import Control.Monad.Metrics (run)

import Katip (
    ColorStrategy(ColorIfTerminal), KatipContext, Severity(EmergencyS, InfoS), Verbosity(V0),
    closeScribes, defaultScribeSettings, initLogEnv, logTM, ls, mkHandleScribe,
    registerScribe, runKatipContextT)
import Control.Monad.Logger.Katip (runKatipLoggingT)

import qualified Kontiki.Raft as K

import qualified Kontiki.CLI.Config as CLI
import Kontiki.Config (Config(Config))
import qualified Kontiki.Config as Config
import Kontiki.Protocol.Types (Node)
import Kontiki.State.Persistent (runPersistentStateT)

main :: CLI.Config -> Node -> IO ()
main cfg node = bracket mkLogEnv closeScribes $ \logEnv -> runKatipContextT logEnv () "main" $ runKatipLoggingT @KatipContext $
    main' `catchAny` reportException
  where
    main' = DB.withDB databasePath options $ \db -> run $ runPersistentStateT db K.initializePersistentState
    databasePath = Text.unpack $ CLI.database $ Config.localNode (Config node cfg)
    options = DB.defaultOptions { DB.createIfMissing = True }
    mkLogEnv = do
        env <- initLogEnv "kontiki-initdb" "production"
        scribe <- mkHandleScribe ColorIfTerminal stderr InfoS V0
        registerScribe "stderr" scribe defaultScribeSettings env
    reportException exc = $(logTM) EmergencyS ("Exception: " <> ls (displayException exc))
