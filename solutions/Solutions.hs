module Solutions
  ( Solution (..),
    IsSlow (..),
    displayAnswer,
    isSolved,
    isSolvedAnswer,
    problemName,
    inputFileName,
    solutions,
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.Typeable (Typeable, cast)
import Day1 (part1, part2)
import Day10 (part1, part2)
import Day11 (part1, part2)
import Day12 (part1, part2)
import Day13 (part1, part2)
import Day14 (part1, part2)
import Day15 (part1, part2)
import Day16 (part1, part2)
import Day17 (part1, part2)
import Day18 (part1, part2)
import Day19 (part1, part2)
import Day2 (part1, part2)
import Day20 (part1, part2)
import Day21 (part1, part2)
import Day22 (part1, part2)
import Day23 (part1, part2)
import Day24 (part1, part2)
import Day25 (part1, part2)
import Day3 (part1, part2)
import Day4 (part1, part2)
import Day5 (part1, part2)
import Day6 (part1, part2)
import Day7 (part1, part2)
import Day8 (part1, part2)
import Day9 (part1, part2)
import Text.Printf (printf)

type IsAnswer a = (NFData a, Show a, Typeable a)

data IsSlow = Slow | NotSlow
  deriving (Show, Eq, Ord)

data Solution = forall a.
  IsAnswer a =>
  MkSolution
  { isSlow :: IsSlow,
    day :: Int,
    part :: Int,
    solve :: (ByteString -> a)
  }

displayAnswer :: (Show a, Typeable a) => a -> String
displayAnswer (cast -> Just ()) = ""
displayAnswer x = show x

isSolvedAnswer :: forall a. Typeable a => a -> Bool
isSolvedAnswer (cast -> Just ()) = False
isSolvedAnswer _ = True

isSolved :: Solution -> Bool
isSolved MkSolution {solve}
  | Just (_ :: ByteString -> ()) <- cast solve = False
  | otherwise = True

problemName :: Solution -> String
problemName MkSolution {day, part} = printf "Day %d part %d" day part

inputFileName :: Solution -> String
inputFileName MkSolution {day} = printf "day%d" day

solutions :: [Solution]
solutions =
  [ MkSolution NotSlow 1 1 Day1.part1,
    MkSolution NotSlow 1 2 Day1.part2,
    MkSolution NotSlow 2 1 Day2.part1,
    MkSolution NotSlow 2 2 Day2.part2,
    MkSolution NotSlow 3 1 Day3.part1,
    MkSolution NotSlow 3 2 Day3.part2,
    MkSolution NotSlow 4 1 Day4.part1,
    MkSolution NotSlow 4 2 Day4.part2,
    MkSolution NotSlow 5 1 Day5.part1,
    MkSolution NotSlow 5 2 Day5.part2,
    MkSolution NotSlow 6 1 Day6.part1,
    MkSolution Slow 6 2 Day6.part2,
    MkSolution NotSlow 7 1 Day7.part1,
    MkSolution NotSlow 7 2 Day7.part2,
    MkSolution NotSlow 8 1 Day8.part1,
    MkSolution NotSlow 8 2 Day8.part2,
    MkSolution NotSlow 9 1 Day9.part1,
    MkSolution NotSlow 9 2 Day9.part2,
    MkSolution NotSlow 10 1 Day10.part1,
    MkSolution NotSlow 10 2 Day10.part2,
    MkSolution NotSlow 11 1 Day11.part1,
    MkSolution NotSlow 11 2 Day11.part2,
    MkSolution NotSlow 12 1 Day12.part1,
    MkSolution NotSlow 12 2 Day12.part2,
    MkSolution NotSlow 13 1 Day13.part1,
    MkSolution NotSlow 13 2 Day13.part2,
    MkSolution NotSlow 14 1 Day14.part1,
    MkSolution NotSlow 14 2 Day14.part2,
    MkSolution NotSlow 15 1 Day15.part1,
    MkSolution NotSlow 15 2 Day15.part2,
    MkSolution NotSlow 16 1 Day16.part1,
    MkSolution NotSlow 16 2 Day16.part2,
    MkSolution NotSlow 17 1 Day17.part1,
    MkSolution NotSlow 17 2 Day17.part2,
    MkSolution NotSlow 18 1 Day18.part1,
    MkSolution NotSlow 18 2 Day18.part2,
    MkSolution NotSlow 19 1 Day19.part1,
    MkSolution NotSlow 19 2 Day19.part2,
    MkSolution NotSlow 20 1 Day20.part1,
    MkSolution NotSlow 20 2 Day20.part2,
    MkSolution NotSlow 21 1 Day21.part1,
    MkSolution NotSlow 21 2 Day21.part2,
    MkSolution NotSlow 22 1 Day22.part1,
    MkSolution NotSlow 22 2 Day22.part2,
    MkSolution NotSlow 23 1 Day23.part1,
    MkSolution NotSlow 23 2 Day23.part2,
    MkSolution NotSlow 24 1 Day24.part1,
    MkSolution NotSlow 24 2 Day24.part2,
    MkSolution NotSlow 25 1 Day25.part1,
    MkSolution NotSlow 25 2 Day25.part2
  ]
