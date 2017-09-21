{-# LANGUAGE TypeFamilies #-}

module Kontiki.Config (
      Config(..)
    , localNode
    ) where

import Control.Lens (to)

import qualified Data.Set as Set

import qualified Data.Text.Lazy as Text

import qualified Kontiki.Raft.Classes.Config as Config

import qualified Kontiki.CLI.Config as CLI
import qualified Kontiki.Protocol.Types as T

data Config = Config { configLocalNode :: T.Node
                     , configCluster :: CLI.Config
                     }
    deriving (Show, Eq)

instance Config.Config Config where
    type Node Config = T.Node

    localNode = to configLocalNode
    nodes = to (Set.fromList . map (T.Node . Text.toStrict . CLI.name) . CLI.nodes . configCluster)

localNode :: Config -> CLI.Node
localNode cfg = case lookup (Text.fromStrict $ T.getNode $ configLocalNode cfg) nodeMap of
    Just n -> n
    Nothing -> error $ "Node " ++ show (T.getNode $ configLocalNode cfg) ++ " not found in configuration"
  where
    nodeMap = map (\n -> (CLI.name n, n)) (CLI.nodes $ configCluster cfg)
