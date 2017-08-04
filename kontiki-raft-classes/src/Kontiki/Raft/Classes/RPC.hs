{-# LANGUAGE TypeFamilies #-}

module Kontiki.Raft.Classes.RPC (
      HasTerm(..)
    ) where

import Kontiki.Raft.Classes.Lens (Lens')

class HasTerm msg where
    type Term msg

    term :: Lens' msg (Term msg)
