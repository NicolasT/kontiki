{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-missing-import-lists -fno-warn-unused-top-binds #-}

module Kontiki.CLI (main) where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText

import Data.Monoid
import Options.Applicative hiding (auto)

import Dhall (auto, detailed, input)

import Kontiki.CLI.Config (Config)
import qualified Kontiki.CLI.InitDB as InitDB
import Kontiki.Protocol.Types (Node(Node))
import qualified Kontiki.Server

withInfo :: Parser a -> String -> ParserInfo a
withInfo p s = info (helper <*> p) $ progDesc s

data Command = InitDB
             | RunNode
    deriving (Show)

parseInitDB :: Parser Command
parseInitDB = pure InitDB

parseNode :: Parser Command
parseNode = pure RunNode

parseCommand :: Parser Command
parseCommand = subparser $ mconcat [
      command "initdb" (parseInitDB `withInfo` "Initialize node database")
    , command "node" (parseNode `withInfo` "Run node")
    ]

data Options = Options { optionsConfig :: FilePath
                       , optionsNodeName :: String
                       , optionsCommand :: Command
                       }
    deriving (Show)

parseOptions :: Parser Options
parseOptions = Options <$> strOption ( short 'c'
                                    <> long "config"
                                    <> metavar "PATH"
                                    <> help "Configuration file"
                                     )
                       <*> strOption ( short 'n'
                                    <> long "node"
                                    <> metavar "NAME"
                                    <> help "Node name"
                                     )
                       <*> parseCommand

opts :: ParserInfo Options
opts = parseOptions `withInfo` "Kontiki - A distributed key-value store"

main :: IO ()
main = execParser opts >>= \(Options configPath node cmd) -> do
    config <- detailed (input auto (LText.pack configPath))
    let node' = Node (Text.pack node)
    case cmd of
        InitDB -> InitDB.main config node'
        RunNode -> Kontiki.Server.main (config :: Config) node'
