{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Kontiki.Raft.Classes.State.Test (tests) where

import Data.Typeable (Typeable)

import Test.Tasty (TestTree, testGroup)

import Kontiki.Raft.Classes.State.Volatile (VolatileState(Index, commitIndex, lastApplied))
import Kontiki.Raft.Classes.Test.Utils (PropLensConstraints, prop_lens, typeId)

tests :: forall volatileState.
         ( Typeable volatileState, VolatileState volatileState
         , PropLensConstraints volatileState (Index volatileState)
         )
      => TestTree
tests = testGroup "Kontiki.Raft.Classes.State" [
      testGroup ("VolatileState @" ++ typeId @volatileState) [
          prop_lens @volatileState "commitIndex" commitIndex
        , prop_lens @volatileState "lastApplied" lastApplied
        ]
    ]
