{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kontiki.Server.Async (
      withAsync
    ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text

import GHC.Conc.Sync (labelThread)

import Control.Concurrent (myThreadId)
import Control.Concurrent.Async.Lifted.Safe (Async, Forall, Pure)
import qualified Control.Concurrent.Async.Lifted.Safe as Async

import Control.Exception (AsyncException(ThreadKilled))
import Control.Exception.Safe (SomeException, displayException, finally, fromException, withException)

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)

import Katip (KatipContext, Namespace(Namespace), Severity(EmergencyS, InfoS, NoticeS), katipAddNamespace, logTM, ls)

withAsync :: ( Monad m
             , MonadIO m
             , MonadBaseControl IO m
             , MonadMask m
             , Forall (Pure m)
             , KatipContext m
             )
          => Text
          -> m a
          -> (Async a -> m b)
          -> m b
withAsync name act cont = flip Async.withAsync cont $ do
    liftIO $ flip labelThread (Text.unpack name) =<< myThreadId

    katipAddNamespace ns $ flip finally logExit
                         $ flip withException logError
                         $ $(logTM) InfoS (name' <> " starting") >> act
  where
    name' = ls $ Text.toTitle name
    ns = Namespace [name]
    logExit = $(logTM) NoticeS (name' <> " quit")
    logError e =
        if not (isThreadKilled e)
            then $(logTM) EmergencyS $ "Exception: " <> ls (displayException (e :: SomeException))
            else $(logTM) InfoS "Thread killed"
    isThreadKilled e = fromException e == Just ThreadKilled
