{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Main where

import           Network.Kontiki.HandleSpec
import           Network.Kontiki.SerializationSpec
import           Test.Tasty
import           Test.Tasty.Hspec


main :: IO ()
main = testGroup "Kontiki Specs" <$> sequence [ testSpec "Serialization" serializationSpec
                                              , testSpec "Handle" handleSpec ]  >>= defaultMain

