{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Kontiki.Raft.Internal.State (
      Some(Some)
    , wrap
    ) where

import Control.Lens (lens, set, view)

import Control.Monad.Indexed.State (IxMonadState, imodify)

import Data.Aeson (ToJSON(toJSON))

import qualified Kontiki.Raft.Classes.State.Volatile as V

data Some s where
    Some :: s r -> Some s

wrap :: IxMonadState m => m (s r) (Some s) ()
wrap = imodify Some

instance (V.VolatileState s, index ~ V.Index s) => V.HasCommitIndex (Some s) index where
    commitIndex = lens
        (\case
            Some s' -> V.dispatch (view V.commitIndex) (view V.commitIndex) (view V.commitIndex) s')
        (\s i -> case s of
            Some s' -> V.dispatch (Some . set V.commitIndex i) (Some . set V.commitIndex i) (Some . set V.commitIndex i) s')

instance (V.VolatileState s, index ~ V.Index s) => V.HasLastApplied (Some s) index where
    lastApplied = lens
        (\case
            Some s' -> V.dispatch (view V.lastApplied) (view V.lastApplied) (view V.lastApplied) s')
        (\s i -> case s of
            Some s' -> V.dispatch (Some . set V.lastApplied i) (Some . set V.lastApplied i) (Some . set V.lastApplied i) s')

instance ( V.VolatileState volatileState
         , ToJSON (volatileState 'V.Follower)
         , ToJSON (volatileState 'V.Candidate)
         , ToJSON (volatileState 'V.Leader))
         => ToJSON (Some volatileState) where
    toJSON (Some s) = V.dispatch toJSON toJSON toJSON s
