module Kontiki.Raft.Classes.Types (
      Index(..)
    , Term(..)
    ) where

class Index i where
    index0 :: i
    succIndex :: i -> i

class Term i where
    term0 :: i
