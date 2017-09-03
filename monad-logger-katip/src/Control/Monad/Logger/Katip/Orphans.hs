{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module:     Control.Monad.Logger.Katip.Orphans
-- Copyright:  (C) 2017 Nicolas Trangez
-- License:    Apache (see the file LICENSE)
--
-- Maintainer: ikke@nicolast.be
-- Stability:  alpha
--
-- Orphan 'MonadLogger' instances for 'KatipT' and 'KatipContextT'.
--
-- See "Control.Monad.Logger.Katip" for more background and examples.

module Control.Monad.Logger.Katip.Orphans () where

import Control.Monad.IO.Class (MonadIO)

import Control.Monad.Logger (MonadLogger(monadLoggerLog))

import Katip (KatipT, KatipContextT)

import Control.Monad.Logger.Katip (defaultMonadLoggerLog, katipLogItem, katipContextLogItem)

instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = defaultMonadLoggerLog $ katipLogItem ()
    {-# INLINE monadLoggerLog #-}

instance MonadIO m => MonadLogger (KatipContextT m) where
    monadLoggerLog = defaultMonadLoggerLog katipContextLogItem
    {-# INLINE monadLoggerLog #-}
