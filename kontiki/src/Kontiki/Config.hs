{-# LANGUAGE TypeFamilies #-}

module Kontiki.Config (
      Config(..)
    ) where

import Control.Lens (to)

import qualified Kontiki.Raft.Classes.Config as Config

import Kontiki.Protocol.Types (Node)

newtype Config = Config { configNode :: Node }
    deriving (Show, Eq)

instance Config.Config Config where
    type Node Config = Node

    localNode = to configNode
