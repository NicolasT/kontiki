{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.State.Volatile (
      VolatileState(..)
    ) where

import Kontiki.Raft.Classes.Lens (Lens')

class VolatileState s where
    type Index s

    commitIndex :: Lens' s (Index s)
    lastApplied :: Lens' s (Index s)
