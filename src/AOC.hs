{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}

module AOC 
  ( Challenge
  ) where

import Day1 (Day1)
import Util (FromArgs (..), RunApp (..))

newtype Challenge
  = D1 Day1
  deriving (Show, Eq)

instance FromArgs Challenge where
    fromArgsIncremental = \case
        "day1":args -> do
            (d1, args) <- fromArgsIncremental args
            Right (D1 d1, args)

        arg:_ -> Left ("unrecognized argument '" ++ arg ++ "'")
        [] -> Left "expected challenge name"

instance RunApp Challenge where
    runApp = \case
        D1 d -> runApp d
