{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Internal.State (
      Role(..)
    , State(..)
    , SomeState(..)
    , role
    , volatileState
    ) where

import Control.Lens ((^.), (&), (.~), lens)

import Data.Text (Text)

import Data.Aeson (ToJSON(toJSON), (.=), object)

import Kontiki.Raft.Classes.State.Volatile (VolatileState(Index, commitIndex, lastApplied))

data Role = Follower
          | Candidate
          | Leader
    deriving (Show, Eq)

data State volatileState volatileLeaderState (r :: Role) where
    F :: !volatileState -> State volatileState volatileLeaderState 'Follower
    C :: !volatileState -> State volatileState volatileLeaderState 'Candidate
    L :: !volatileState -> !volatileLeaderState -> State volatileState volatileLeaderState 'Leader

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

instance (ToJSON volatileState, ToJSON volatileLeaderState) => ToJSON (State volatileState volatileLeaderState r) where
    toJSON s = case s of
        F v -> object [ "role" .= ("follower" :: Text)
                      , "volatileState" .= v
                      ]
        C v -> object [ "role" .= ("candidate" :: Text)
                      , "volatileState" .= v
                      ]
        L v l -> object [ "role" .= ("leader" :: Text)
                        , "volatileState" .= v
                        , "volatileLeaderState" .= l
                        ]


data SomeState volatileState volatileLeaderState where
    SomeState :: !(State volatileState volatileLeaderState r) -> SomeState volatileState volatileLeaderState

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

instance (ToJSON volatileState, ToJSON volatileLeaderState) => ToJSON (SomeState volatileState volatileLeaderState) where
    toJSON (SomeState s) = toJSON s


role :: SomeState volatileState volatileLeaderState -> Role
role (SomeState s) = case s of
    F _ -> Follower
    C _ -> Candidate
    L _ _ -> Leader

volatileState :: SomeState volatileState volatileLeaderState -> volatileState
volatileState (SomeState s) = case s of
    F v -> v
    C v -> v
    L v _ -> v
