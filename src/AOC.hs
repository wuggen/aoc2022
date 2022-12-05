{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}

module AOC 
  ( Challenge
  ) where

import Day1 (Day1)
import Day2 (Day2)
import Util (FromArgs (..), RunApp (..))

data Challenge
  = D1 Day1
  | D2 Day2
  deriving (Show, Eq)

instance FromArgs Challenge where
    fromArgsIncremental = \case
        "day1":args -> do
            (d1, args) <- fromArgsIncremental args
            Right (D1 d1, args)

        "day2":args -> do
            (d2, args) <- fromArgsIncremental args
            Right (D2 d2, args)

        arg:_ -> Left ("unrecognized argument '" ++ arg ++ "'")
        [] -> Left "expected challenge name"

instance RunApp Challenge where
    runApp = \case
        D1 d -> runApp d
        D2 d -> runApp d
