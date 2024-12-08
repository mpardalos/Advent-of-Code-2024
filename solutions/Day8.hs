module Day8 (part1, part2) where

import Data.Array.Unboxed (UArray, assocs, bounds, inRange)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Util (orderedPairs, readDenseGrid)

type Location = (Int, Int)

type Antenna = Location

type Antinode = Location

type Bounds = (Location, Location)

readInput :: ByteString -> (Bounds, Map Char [Antenna])
readInput input =
  let grid = readDenseGrid @UArray input
   in ( bounds grid,
        assocs grid
          & map (\(a, b) -> (b, [a]))
          & Map.fromListWith (++)
          & Map.delete '.'
      )

-- Infinite list of antinodes, towards the second antenna
antinodesRight :: Antenna -> Antenna -> [Antinode]
antinodesRight (r1, c1) (r2, c2) =
  let rowDistance = r2 - r1
      colDistance = c2 - c1
   in [(r2 + n * rowDistance, c2 + n * colDistance) | n <- [0 ..]]

part1 :: ByteString -> Int
part1 input =
  let (gridBounds, locations) = readInput input
   in locations
        -- Take orderedPairs (instead of uniquePairs), so that we get each pair
        -- in both orientations (antinodesRight only gives antinodes towards the
        -- second antenna)
        & Map.map (map (uncurry antinodesRight) . orderedPairs)
        -- Antinode 0 for each pair is on the second antenna, we want only the
        -- one after that. We filter later down to only keep in-bounds antinodes
        & Map.map (map (!! 1))
        & Map.elems
        & concat
        & filter (inRange gridBounds)
        & Set.fromList
        & length

part2 :: ByteString -> Int
part2 input =
  let (gridBounds, locations) = readInput input
   in locations
        & Map.map orderedPairs
        & Map.map (map (uncurry antinodesRight))
        -- For part 2 we want all in-bounds antinodes, including the 0th one
        -- which matches with the second node.
        & Map.map (concatMap (takeWhile (inRange gridBounds)))
        & Map.elems
        & concat
        & Set.fromList
        & length
