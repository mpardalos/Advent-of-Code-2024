{-# LANGUAGE MultiWayIf #-}

module Day11 (part1, part2) where

import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.MemoTrie (memo2)
import Math.NumberTheory.Logarithms (integerLog10')
import Util (readSpacedInts)

splitDigits :: Int -> Maybe (Int, Int)
splitDigits n =
  let digitCount = integerLog10' (fromIntegral n) + 1
      middleDigit = digitCount `div` 2
   in if even digitCount
        then Just (n `div` (10 ^ middleDigit), n `mod` (10 ^ middleDigit))
        else Nothing

countAfter :: Int -> Int -> Int
countAfter =
  memo2 $ \steps x ->
    if
      | steps == 0 ->
          1
      | x == 0 ->
          countAfter (steps - 1) 1
      | Just (l, r) <- splitDigits x ->
          countAfter (steps - 1) l
            + countAfter (steps - 1) r
      | otherwise ->
          countAfter (steps - 1) (2024 * x)

part1 :: ByteString -> Int
part1 input =
  readSpacedInts input
    & map (countAfter 25)
    & sum

part2 :: ByteString -> Int
part2 input =
  readSpacedInts input
    & map (countAfter 75)
    & sum
