{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.State (
      Role(..)
    , State(..)
    , SomeState(..)
    ) where

import Control.Lens ((^.), (&), (.~), lens)

import Kontiki.Raft.Classes.State.Volatile (VolatileState(Index, commitIndex, lastApplied))

data Role = Follower
          | Candidate
          | Leader

data State volatileState volatileLeaderState (r :: Role) where
    F :: volatileState -> State volatileState volatileLeaderState 'Follower
    C :: volatileState -> State volatileState volatileLeaderState 'Candidate
    L :: volatileState -> volatileLeaderState -> State volatileState volatileLeaderState 'Leader

deriving instance (Show volatileState, Show volatileLeaderState) => Show (State volatileState volatileLeaderState r)
deriving instance (Eq volatileState, Eq volatileLeaderState) => Eq (State volatileState volatileLeaderState r)

instance VolatileState volatileState => VolatileState (State volatileState volatileLeaderState r) where
    type Index (State volatileState volatileLeaderState r) = Index volatileState

    commitIndex = lens
        (\case
            F v -> v ^. commitIndex
            C v -> v ^. commitIndex
            L v _ -> v ^. commitIndex)
        (\s i -> case s of
            F v -> F $ v & commitIndex .~ i
            C v -> C $ v & commitIndex .~ i
            L v l -> L (v & commitIndex .~ i) l)

    lastApplied = lens
        (\case
            F v -> v ^. lastApplied
            C v -> v ^. lastApplied
            L v _ -> v ^. lastApplied)
        (\s i -> case s of
            F v -> F $ v & lastApplied .~ i
            C v -> C $ v & lastApplied .~ i
            L v l -> L (v & lastApplied .~ i) l)



data SomeState volatileState volatileLeaderState where
    SomeState :: State volatileState volatileLeaderState r -> SomeState volatileState volatileLeaderState

deriving instance (Show volatileState, Show volatileLeaderState) => Show (SomeState volatileState volatileLeaderState)
instance (Eq volatileState, Eq volatileLeaderState) => Eq (SomeState volatileState volatileLeaderState) where
    a == b = case a of
        SomeState a' -> case b of
            SomeState b' -> case a' of
                F _ -> case b' of
                    F _ -> a' == b'
                    _ -> False
                C _ -> case b' of
                    C _ -> a' == b'
                    _ -> False
                L _ _ -> case b' of
                    L _ _ -> a' == b'
                    _ -> False
