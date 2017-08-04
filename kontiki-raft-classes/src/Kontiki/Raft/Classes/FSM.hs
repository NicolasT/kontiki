{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.FSM (
      MonadFSM(..)
    ) where

class MonadFSM m where
    type Entry m

    applyEntry :: Entry m -> m ()
