{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7 (part1, part2) where

import Control.Monad (guard)
import Control.Parallel.Strategies (parMap, rpar)
import Data.Attoparsec.ByteString.Char8 (char, decimal, sepBy, string)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.List.Extra (stripSuffix)
import Data.Maybe (catMaybes)
import Util (linesOf, parseOrError)

data Equation = Equation
  { testValue :: !Int,
    parts :: ![Int]
  }
  deriving (Eq, Ord, Show)

type UnOperator = Int -> Int -> Maybe Int

readEquations :: ByteString -> [Equation]
readEquations = parseOrError $ linesOf $ do
  testValue <- decimal
  _ <- string ": "
  parts <- decimal `sepBy` char ' '
  return Equation {testValue, parts}

isResultPossible :: [UnOperator] -> Int -> [Int] -> Bool
isResultPossible unOperators fullTarget = go fullTarget . reverse
  where
    go _ [] = False
    go target [part] = part == target
    go target (part : parts) =
      [unop target part | unop <- unOperators]
        & catMaybes
        & any (`go` parts)

equationContribution :: [UnOperator] -> Equation -> Int
equationContribution operators Equation {testValue, parts}
  | isResultPossible operators testValue parts = testValue
  | otherwise = 0

unMultiply, unPlus, unConcat :: UnOperator
unMultiply target x = do
  guard (target `mod` x == 0)
  Just (target `div` x)
unPlus target x = do
  guard (target >= x)
  Just (target - x)
unConcat target x =
  stripSuffix (show x) (show target) >>= \case
    "" -> Just 0
    str -> Just (read str)

part1 :: ByteString -> Int
part1 input =
  readEquations input
    & parMap rpar (equationContribution [unMultiply, unPlus])
    & sum

part2 :: ByteString -> Int
part2 input =
  readEquations input
    & parMap rpar (equationContribution [unMultiply, unPlus, unConcat])
    & sum
