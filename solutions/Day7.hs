{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day7 (part1, part2) where

import Data.Attoparsec.ByteString.Char8 (char, decimal, sepBy, string)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Debug.Trace (trace, traceShow)
import GHC.Records (getField)
import Text.Printf (printf)
import Util (linesOf, parseOrError)

data Equation = Equation
  { testValue :: !Int,
    parts :: ![Int]
  }
  deriving (Eq, Ord, Show)

readEquations :: ByteString -> [Equation]
readEquations = parseOrError $ linesOf $ do
  testValue <- decimal
  _ <- string ": "
  parts <- decimal `sepBy` char ' '
  return Equation {..}

possibleResults :: [Int] -> [Int]
possibleResults = go
  where
    go xs | trace ("go " ++ show xs) False = undefined
    go [] = []
    go [x] = [x]
    go (x : xs) = do
      operator <-
        [ \l r -> trace (printf "\n%d * %d = %d" l r (l * r)) (l * r),
          \l r -> trace (printf "\n%d + %d = %d" l r (l + r)) (l + r)
          ]
      rest <- possibleResults xs
      return (operator x rest)

couldBeTrue :: Equation -> Bool
couldBeTrue Equation {testValue, parts} = testValue `elem` possibleResults parts

part1 :: ByteString -> Int
part1 input =
  readEquations input
    & filter couldBeTrue
    & map (getField @"testValue")
    & sum

part2 :: ByteString -> ()
part2 _ = ()
