{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.Config (
      Config(..)
    ) where

import Kontiki.Raft.Classes.Lens (Getter)

class Config c where
    type Node c

    localNode :: Getter c (Node c)
