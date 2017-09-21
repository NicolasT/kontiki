{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.Config (
      Config(..)
    ) where

import Data.Set (Set)

import Kontiki.Raft.Classes.Lens (Getter)

class Config c where
    type Node c

    localNode :: Getter c (Node c)
    nodes :: Getter c (Set (Node c))
