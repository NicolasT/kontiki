{-# LANGUAGE Rank2Types #-}

-- |
-- Module:      Kontiki.Raft.Classes.Lens
-- Copyright:   (C) 2017 Nicolas Trangez
-- License:     Apache (see the file LICENSE)
-- Maintainer:  Nicolas Trangez <ikke@nicolast.be>
-- Stability:   provisional
-- Portability: Rank2Types
--
-- This module exports a couple of basic 'Lens' type definitions.

module Kontiki.Raft.Classes.Lens (
      Lens'
    ) where

-- | The standard 'Lens' definition
type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

-- | A simple 'Lens'
type Lens' s a = Lens s s a a
