{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Mock (
      PersistentStateAction(..)
    ) where

import Data.Type.Equality ((:~:)(Refl))

import Control.Monad.Mock (Action(eqAction), MockT, mockAction)

import Kontiki.Raft.Classes.State.Persistent (
    MonadPersistentState(Term, Node, Entry, Index,
                         getCurrentTerm, setCurrentTerm,
                         getVotedFor, setVotedFor,
                         getLogEntry, setLogEntry, lastLogEntry))

import qualified Kontiki.Raft.Types as T

data PersistentStateAction r where
    GetCurrentTerm :: PersistentStateAction T.Term
    SetCurrentTerm :: T.Term -> PersistentStateAction ()
    GetVotedFor :: PersistentStateAction (Maybe T.Node)
    SetVotedFor :: Maybe T.Node -> PersistentStateAction ()
    GetLogEntry :: T.Index -> PersistentStateAction (T.Term, Int)
    SetLogEntry :: T.Index -> T.Term -> Int -> PersistentStateAction ()
    LastLogEntry :: PersistentStateAction (Maybe (T.Index, T.Term, Int))

deriving instance Eq (PersistentStateAction r)
deriving instance Show (PersistentStateAction r)

instance Action PersistentStateAction where
    eqAction GetCurrentTerm GetCurrentTerm = Just Refl
    eqAction (SetCurrentTerm a) (SetCurrentTerm b) = if a == b then Just Refl else Nothing
    eqAction GetVotedFor GetVotedFor = Just Refl
    eqAction (SetVotedFor a) (SetVotedFor b) = if a == b then Just Refl else Nothing
    eqAction (GetLogEntry a) (GetLogEntry b) = if a == b then Just Refl else Nothing
    eqAction (SetLogEntry a1 a2 a3) (SetLogEntry b1 b2 b3) = if a1 == b1 && a2 == b2 && a3 == b3 then Just Refl else Nothing
    eqAction LastLogEntry LastLogEntry = Just Refl
    eqAction _ _ = Nothing

instance Monad m => MonadPersistentState (MockT PersistentStateAction m) where
    type Term (MockT PersistentStateAction m) = T.Term
    type Node (MockT PersistentStateAction m) = T.Node
    type Entry (MockT PersistentStateAction m) = Int
    type Index (MockT PersistentStateAction m) = T.Index

    getCurrentTerm = mockAction "getCurrentTerm" GetCurrentTerm
    setCurrentTerm = mockAction "setCurrentTerm" . SetCurrentTerm
    getVotedFor = mockAction "getVotedFor" GetVotedFor
    setVotedFor = mockAction "setVotedFor" . SetVotedFor
    getLogEntry = mockAction "getLogEntry" . GetLogEntry
    setLogEntry i t e = mockAction "setLogEntry" $ SetLogEntry i t e
    lastLogEntry = mockAction "lastLogEntry" LastLogEntry

