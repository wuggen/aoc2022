{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Day1
  ( Day1
  , splitEmpty
  ) where

import Util (FromArgs (..), RunApp (..))
import Data.List (sort)

newtype Day1 = Day1 String deriving (Show, Eq)

instance FromArgs Day1 where
    fromArgsIncremental = \case
        [] -> Left "expected input file name"
        fname:_ -> Right (Day1 fname, [])

instance RunApp Day1 where
    runApp = run

splitEmpty :: [[a]] -> [[[a]]]
splitEmpty = se [] 
  where
    se :: [[a]] -> [[a]] -> [[[a]]]
    se acc = \case
        [] -> [acc]
        []:ls -> acc : se [] ls
        l:ls -> se (l:acc) ls

totals :: String -> [Integer]
totals contents = reverse . sort . map sum $ intSections
  where
    sections = splitEmpty (lines contents)
    intSections = map (map read) sections

run :: Day1 -> IO ()
run (Day1 fname) = do
    contents <- readFile fname
    let vals = take 3 (totals contents)
    putStr "Maximum value: "
    print (head vals)
    putStr "Sum of top three values: "
    print (sum vals)
