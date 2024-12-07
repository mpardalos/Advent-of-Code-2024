{-# LANGUAGE OverloadedStrings #-}

module Day7 (part1, part2) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Attoparsec.ByteString.Char8 (char, decimal, sepBy, string)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Util (linesOf, parseOrError)
import Math.NumberTheory.Logarithms (integerLog10, integerLog10')

data Equation = Equation
  { testValue :: !Int,
    parts :: ![Int]
  }
  deriving (Eq, Ord, Show)

type Operator = Int -> Int -> Int

readEquations :: ByteString -> [Equation]
readEquations = parseOrError $ linesOf $ do
  testValue <- decimal
  _ <- string ": "
  parts <- decimal `sepBy` char ' '
  return Equation {testValue, parts}

possibleResults :: [Operator] -> [Int] -> [Int]
possibleResults operators = go . reverse
  where
    go [] = []
    go [x] = [x]
    go (x : xs) = do
      op <- operators
      rest <- go xs
      -- Order is important here. We run the list in reverse, so we must reverse
      -- the arguments here again
      return (rest `op` x)

equationContribution :: [Operator] -> Equation -> Int
equationContribution operators Equation {testValue, parts}
  | testValue `elem` possibleResults operators parts = testValue
  | otherwise = 0

intConcat :: Int -> Int -> Int
intConcat l r = (l * (10 ^ (1 + integerLog10' (fromIntegral r)))) + r

part1 :: ByteString -> Int
part1 input =
  readEquations input
    & parMap rpar (equationContribution [(*), (+)])
    & sum

part2 :: ByteString -> Int
part2 input =
  readEquations input
    & parMap rpar (equationContribution [(*), (+), intConcat])
    & sum
