{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.State.Volatile (
      VolatileState(..)
    , VolatileCandidateState(..)
    ) where

import Data.Set (Set)

import Kontiki.Raft.Classes.Lens (Lens')

class VolatileState s where
    type Index s

    commitIndex :: Lens' s (Index s)
    lastApplied :: Lens' s (Index s)

class VolatileCandidateState s where
    type Node s

    votesGranted :: Lens' s (Set (Node s))
