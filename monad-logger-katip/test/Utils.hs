{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Utils (
      withLogEnv
    , summarizeLogs
    ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (TVar, newTVarIO, modifyTVar', readTVarIO)
import Control.Monad (when)
import Data.Monoid ((<>))

import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)

import Data.Aeson (Object)

import Katip (
    Item(_itemMessage, _itemNamespace, _itemPayload, _itemSeverity), LogEnv, LogItem,
    LogStr(unLogStr), Namespace, Scribe(Scribe), Severity, Verbosity,
    defaultScribeSettings, initLogEnv, permitItem, registerScribe, toObject)

withLogEnv :: Severity -> Verbosity -> ((LogEnv, TVar [Item Object]) -> IO ()) -> IO ()
withLogEnv sev _ver act = do
    tvar <- newTVarIO []
    logEnv <- registerScribe "memory" (memoryScribe tvar) defaultScribeSettings =<< initLogEnv "monad-logger-katip" "testing"
    act (logEnv, tvar)
  where
    liPush :: forall a. LogItem a => TVar [Item Object] -> Item a -> IO ()
    liPush tvar i = when (permitItem sev i) $
        atomically $ modifyTVar' tvar (<> [toObject <$> i])
    memoryScribe tvar = Scribe (liPush tvar) (return ())

summarizeLogs :: TVar [Item Object] -> IO [(Severity, Text, Namespace, Object)]
summarizeLogs tvar = map summarizeItem <$> readTVarIO tvar
  where
    summarizeItem i = (_itemSeverity i, toLazyText $ unLogStr $ _itemMessage i, _itemNamespace i, _itemPayload i)
