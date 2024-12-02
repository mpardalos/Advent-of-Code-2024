module Day2 (part1, part2) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Function ((&))
import Util (readSpacedInts)

allIncreasing :: [Int] -> Bool
allIncreasing (x1:x2:xs) = x2 > x1 && allIncreasing (x2:xs)
allIncreasing _ = True

allDecreasing :: [Int] -> Bool
allDecreasing (x1:x2:xs) = x2 < x1 && allDecreasing (x2:xs)
allDecreasing _ = True

differencesSafe :: [Int] -> Bool
differencesSafe (x1:x2:xs) =
  let diff = abs (x2 - x1)
  in (1 <= diff && diff <= 3) && differencesSafe (x2:xs)
differencesSafe _ = True

isSafe :: [Int] -> Bool
isSafe xs = (allIncreasing xs || allDecreasing xs) && differencesSafe xs

dropping1 :: [Int] -> [[Int]]
dropping1 [] = [[]]
dropping1 (x:xs) = xs : map (x:) (dropping1 xs)

part1 :: ByteString -> Int
part1 input =
  BS.lines input
  & map readSpacedInts
  & filter isSafe
  & length

part2 :: ByteString -> Int
part2 input =
  BS.lines input
  & map readSpacedInts
  & filter (any isSafe . dropping1)
  & length
