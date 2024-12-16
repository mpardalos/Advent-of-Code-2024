{-# LANGUAGE NoMonomorphismRestriction #-}

module Day16 (part1, part2) where

import Algorithm.Search (dijkstra)
import Data.Array.IArray qualified as Array
import Data.ByteString (ByteString)
import Safe (fromJustNote)
import Util (Coords, Grid, east, findAllIndices, north, readDenseGrid, south, west)

type Maze = Grid Char

readInput :: ByteString -> (Coords, Coords, Maze)
readInput input =
  let g = readDenseGrid input
   in ( head $ findAllIndices (== 'S') g,
        head $ findAllIndices (== 'E') g,
        g
      )

data Direction = N | E | S | W
  deriving (Eq, Ord, Show)

type State = (Coords, Direction)

neighbours :: Maze -> State -> [State]
neighbours m =
  filter (\(c, _) -> (m Array.!? c) `elem` [Just 'S', Just 'E', Just '.'])
    . ( \case
          (c, N) -> [(north c 1, N), (c, E), (c, W)]
          (c, E) -> [(east c 1, E), (c, N), (c, S)]
          (c, S) -> [(south c 1, S), (c, W), (c, E)]
          (c, W) -> [(west c 1, W), (c, S), (c, N)]
      )

stateDistance :: State -> State -> Int
stateDistance s1@((r1, c1), d1) s2@((r2, c2), d2)
  | d1 == d2 && c1 == c2 = abs (r2 - r1)
  | d1 == d2 && r1 == r2 = abs (c2 - c1)
  | d1 /= d2 = 1000
  | otherwise = error ("Non-adjacent states " ++ show s1 ++ " and " ++ show s2)

part1 :: ByteString -> Int
part1 input =
  let (start, end, grid) = readInput input
      (cost, _path) =
        fromJustNote "No path" $
          dijkstra (neighbours grid) stateDistance ((== end) . fst) (start, E)
   in cost

part2 :: ByteString -> ()
part2 _ = ()
