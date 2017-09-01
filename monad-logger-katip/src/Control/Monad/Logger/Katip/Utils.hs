{-# LANGUAGE TemplateHaskell #-}

module Control.Monad.Logger.Katip.Utils (
      location
    ) where

import Language.Haskell.TH.Syntax (Exp, Loc(Loc), Q, lift, qLocation)

-- | Retrieve current file location
location :: Q Exp
location = [| $(qLocation >>= liftLoc) |]

liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) =
    [| Loc $(lift a) $(lift b) $(lift c) ($(lift d1), $(lift d2)) ($(lift e1), $(lift e2)) |]
