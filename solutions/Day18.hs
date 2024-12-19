module Day18 (part1, part2) where

import Algorithm.Search (aStar)
import Data.Array.IArray (IArray (bounds), Ix (inRange, range), array, (!), (//))
import Data.Attoparsec.ByteString.Char8 (char, decimal)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Maybe (isJust)
import Safe (fromJustNote)
import Util (Coords, UGrid, east, linesOf, manhattanDistance, north, parseOrError, south, west)

type Bounds = (Coords, Coords)

readInput :: ByteString -> [Coords]
readInput = parseOrError . linesOf $ do
  row <- decimal
  _ <- char ','
  col <- decimal
  return (row, col)

mkGrid :: Bounds -> [Coords] -> UGrid Bool
mkGrid bs cs =
  array bs [(c, False) | c <- range bs] // [(c, True) | c <- cs]

neighbours :: UGrid Bool -> Coords -> [Coords]
neighbours g c =
  [north c 1, east c 1, south c 1, west c 1]
    & filter (inRange (bounds g))
    & filter (\c' -> not (g ! c'))

exitSteps :: UGrid Bool -> Maybe Int
exitSteps g =
  let (start, end) = bounds g
   in fst <$> aStar (neighbours g) (\_ _ -> 1) (manhattanDistance end) (== end) start

part1 :: ByteString -> Int
part1 input =
  readInput input
    & take 1024
    & mkGrid ((0, 0), (70, 70))
    & exitSteps
    & fromJustNote "No path"

minSolvable :: [Coords] -> Coords
minSolvable coords = go 0 (length coords - 1)
  where
    solvable n =
      take n coords
        & mkGrid ((0, 0), (70, 70))
        & exitSteps
        & isJust

    go lo hi
      | hi == lo + 1 = coords !! lo
      | solvable ((hi + lo) `div` 2) = go ((hi + lo) `div` 2) hi
      | otherwise = go lo ((hi + lo) `div` 2)

part2 :: ByteString -> Coords
part2 = minSolvable . readInput
