{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Kontiki.Raft.Classes.State.Volatile (
    VolatileState(Index, commitIndex, lastApplied),
    VolatileCandidateState(Node, votesGranted))

data Role = Follower
          | Candidate
          | Leader
    deriving (Show, Eq)

data State volatileState volatileCandidateState volatileLeaderState (r :: Role) where
    F :: !volatileState -> State volatileState volatileCandidateState volatileLeaderState 'Follower
    C :: !volatileState -> !volatileCandidateState -> State volatileState volatileCandidateState volatileLeaderState 'Candidate
    L :: !volatileState -> !volatileLeaderState -> State volatileState volatileCandidateState volatileLeaderState 'Leader

deriving instance (Show volatileState, Show volatileCandidateState, Show volatileLeaderState) => Show (State volatileState volatileCandidateState volatileLeaderState r)
deriving instance (Eq volatileState, Eq volatileCandidateState, Eq volatileLeaderState) => Eq (State volatileState volatileCandidateState volatileLeaderState r)

instance VolatileState volatileState => VolatileState (State volatileState volatileCandidateState volatileLeaderState r) where
    type Index (State volatileState volatileCandidateState volatileLeaderState r) = Index volatileState

    commitIndex = lens
        (\case
            F v -> v ^. commitIndex
            C v _ -> v ^. commitIndex
            L v _ -> v ^. commitIndex)
        (\s i -> case s of
            F v -> F $ v & commitIndex .~ i
            C v c -> C (v & commitIndex .~ i) c
            L v l -> L (v & commitIndex .~ i) l)

    lastApplied = lens
        (\case
            F v -> v ^. lastApplied
            C v _ -> v ^. lastApplied
            L v _ -> v ^. lastApplied)
        (\s i -> case s of
            F v -> F $ v & lastApplied .~ i
            C v c -> C (v & lastApplied .~ i) c
            L v l -> L (v & lastApplied .~ i) l)

instance VolatileCandidateState volatileCandidateState => VolatileCandidateState (State volatileState volatileCandidateState volatileLeaderState 'Candidate) where
    type Node (State volatileState volatileCandidateState volatileLeaderState 'Candidate) = Node volatileCandidateState

    votesGranted = lens
        (\case
             C _ c -> c ^. votesGranted)
        (\s i -> case s of
             C v c -> C v (c & votesGranted .~ i))

instance (ToJSON volatileState, ToJSON volatileCandidateState, ToJSON volatileLeaderState) => ToJSON (State volatileState volatileCandidateState volatileLeaderState r) where
    toJSON s = case s of
        F v -> object [ "role" .= ("follower" :: Text)
                      , "volatileState" .= v
                      ]
        C v c -> object [ "role" .= ("candidate" :: Text)
                        , "volatileState" .= v
                        , "volatileCandidateState" .= c
                        ]
        L v l -> object [ "role" .= ("leader" :: Text)
                        , "volatileState" .= v
                        , "volatileLeaderState" .= l
                        ]


data SomeState volatileState volatileCandidateState volatileLeaderState where
    SomeState :: !(State volatileState volatileCandidateState volatileLeaderState r) -> SomeState volatileState volatileCandidateState volatileLeaderState

deriving instance (Show volatileState, Show volatileCandidateState, Show volatileLeaderState) => Show (SomeState volatileState volatileCandidateState volatileLeaderState)
instance (Eq volatileState, Eq volatileCandidateState, Eq volatileLeaderState) => Eq (SomeState volatileState volatileCandidateState volatileLeaderState) where
    a == b = case a of
        SomeState a' -> case b of
            SomeState b' -> case a' of
                F _ -> case b' of
                    F _ -> a' == b'
                    _ -> False
                C _ _ -> case b' of
                    C _ _ -> a' == b'
                    _ -> False
                L _ _ -> case b' of
                    L _ _ -> a' == b'
                    _ -> False

instance (ToJSON volatileState, ToJSON volatileCandidateState, ToJSON volatileLeaderState) => ToJSON (SomeState volatileState volatileCandidateState volatileLeaderState) where
    toJSON (SomeState s) = toJSON s


role :: SomeState volatileState volatileCandidateState volatileLeaderState -> Role
role (SomeState s) = case s of
    F _ -> Follower
    C _ _ -> Candidate
    L _ _ -> Leader

volatileState :: SomeState volatileState volatileCandidateState volatileLeaderState -> volatileState
volatileState (SomeState s) = case s of
    F v -> v
    C v _ -> v
    L v _ -> v
