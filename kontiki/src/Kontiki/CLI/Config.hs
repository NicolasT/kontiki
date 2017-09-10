{-# LANGUAGE DeriveGeneric #-}

module Kontiki.CLI.Config (
      Config(..)
    , Node(..)
    ) where

import GHC.Generics (Generic)

import Dhall (Interpret, Natural, Text)

data Node = Node { name :: Text
                 , host :: Text
                 , port :: Natural
                 , database :: Text
                 }
    deriving (Show, Eq, Generic)

instance Interpret Node

data Config = Config { clusterId :: Text
                     , nodes :: [Node]
                     }
    deriving (Show, Eq, Generic)

instance Interpret Config
