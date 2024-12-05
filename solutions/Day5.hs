module Day5 (part1, part2) where

import Data.Attoparsec.ByteString.Char8 (char, decimal, endOfLine, sepBy1)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.List (sortBy)
import Data.Set (Set)
import Data.Set qualified as Set
import Text.Printf (printf)
import Util (parseOrError, linesOf)

type Rules = Set (Int, Int)

type Update = [Int]

readInput :: ByteString -> (Rules, [Update])
readInput = parseOrError $ do
  rules <- linesOf $ do
    n1 <- decimal
    _ <- char '|'
    n2 <- decimal
    return (n1, n2)
  endOfLine
  endOfLine
  updates <- linesOf (decimal `sepBy1` char ',')
  return (Set.fromList rules, updates)

comparingRules :: Rules -> Int -> Int -> Ordering
comparingRules rules n1 n2
  | n1 == n2 = EQ
  | Set.member (n1, n2) rules = LT
  | Set.member (n2, n1) rules = GT
  | otherwise = error (printf "Missing case: %d, %d" n1 n2)

isValid :: Rules -> Update -> Bool
isValid rules update = update == sortBy (comparingRules rules) update

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

part1 :: ByteString -> Int
part1 input =
  let (rules, updates) = readInput input
   in filter (isValid rules) updates
        & map middle
        & sum

part2 :: ByteString -> Int
part2 input =
  let (rules, updates) = readInput input
   in updates
        & filter (not . isValid rules)
        & map (sortBy (comparingRules rules))
        & map middle
        & sum
