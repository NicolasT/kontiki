{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Kontiki.Raft.Orphans () where

import Control.Monad.Logger (NoLoggingT)

import Kontiki.Raft.Classes.State.Persistent (MonadPersistentState(Node, Term, Index, Entry))

instance (Monad m, MonadPersistentState m) => MonadPersistentState (NoLoggingT m) where
    type Term (NoLoggingT m) = Term m
    type Node (NoLoggingT m) = Node m
    type Entry (NoLoggingT m) = Entry m
    type Index (NoLoggingT m) = Index m
