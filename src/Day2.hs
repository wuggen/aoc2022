{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}

module Day2
  ( Day2
  ) where
import Util

data Day2
  = D2Part1 String
  | D2Part2 String
  deriving (Show, Eq)

fname :: Day2 -> String
fname (D2Part1 s) = s
fname (D2Part2 s) = s

instance FromArgs Day2 where
    fromArgsIncremental = \case
        "part1":fname:args -> Right (D2Part1 fname, args)
        "part2":fname:args -> Right (D2Part2 fname, args)
        _ -> Left "expected part1 or part2, and input file name"

instance RunApp Day2 where
    runApp = run

class Score a where
    score :: a -> Int

data RPS
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq)

instance Score RPS where
    score = \case
        Rock -> 1
        Paper -> 2
        Scissors -> 3

data Outcome
  = LeftWins
  | Draw
  | RightWins
  deriving (Show, Eq, Ord)

instance Score Outcome where
    score = \case
        LeftWins -> 0
        Draw -> 3
        RightWins -> 6

instance Score (RPS, RPS) where
    score (l, r) = score r + score (l `against` r)

instance Score (RPS, Outcome) where
    score (l, o) = score (l, r)
      where r = l `withOutcome` o

against :: RPS -> RPS -> Outcome
left `against` right
    | left == right = Draw
    | defeaterOf left == right = RightWins
    | otherwise = LeftWins

defeaterOf :: RPS -> RPS
defeaterOf = \case
    Rock -> Paper
    Paper -> Scissors
    Scissors -> Rock

loserOf :: RPS -> RPS
loserOf = \case
    Rock -> Scissors
    Paper -> Rock
    Scissors -> Paper

withOutcome :: RPS -> Outcome -> RPS
withOutcome left = \case
    LeftWins -> loserOf left
    Draw -> left
    RightWins -> defeaterOf left

class FromChar a where
    fromChar :: Char -> a

instance FromChar RPS where
    fromChar c
        | c `elem` ['A', 'X'] = Rock
        | c `elem` ['B', 'Y'] = Paper
        | c `elem` ['C', 'Z'] = Scissors
        | otherwise = error ("invalid char " ++ show c)

instance FromChar Outcome where
    fromChar = \case
        'X' -> LeftWins
        'Y' -> Draw
        'Z' -> RightWins
        c -> error ("invalid char " ++ show c)

pairFromStr :: (FromChar a, FromChar b) => String -> (a, b)
pairFromStr [l, ' ', r] = (fromChar l, fromChar r)
pairFromStr _ = error "malformed line"

pairsFromStr :: (FromChar a, FromChar b) => String -> [(a, b)]
pairsFromStr = map pairFromStr . lines

totalScore :: Score a => [a] -> Int
totalScore = sum . map score

run :: Day2 -> IO ()
run d2 = do
    contents <- readFile (fname d2)
    let s = case d2 of
              D2Part1 _ -> totalScore (pairsFromStr contents :: [(RPS, RPS)])
              D2Part2 _ -> totalScore (pairsFromStr contents :: [(RPS, Outcome)])
    putStr "Total score: "
    print s
