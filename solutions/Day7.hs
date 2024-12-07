{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day7 (part1, part2) where

import Control.Parallel.Strategies (parMap, rpar)
import Data.Attoparsec.ByteString.Char8 (char, decimal, sepBy, string)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Util (linesOf, parseOrError)

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

couldBeTrue :: [Operator] -> Equation -> Bool
couldBeTrue operators Equation {testValue, parts} = testValue `elem` possibleResults operators parts

part1 :: ByteString -> Int
part1 input =
  readEquations input
    & parMap
      rpar
      ( \eq ->
          if couldBeTrue [(*), (+)] eq
            then eq.testValue
            else 0
      )
    & sum

intConcat :: Int -> Int -> Int
intConcat l r = (l * (10 ^ digitCount r)) + r
  where
    digitCount :: Int -> Int
    digitCount n
      | n `div` 10 == 0 = 1
      | otherwise = 1 + digitCount (n `div` 10)

part2 :: ByteString -> Int
part2 input =
  readEquations input
    & parMap
      rpar
      ( \eq ->
          if couldBeTrue [(*), (+), intConcat] eq
            then eq.testValue
            else 0
      )
    & sum
