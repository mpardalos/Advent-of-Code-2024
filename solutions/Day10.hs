{-# LANGUAGE LambdaCase #-}

module Day10 (part1, part2) where

import Data.Array ((!), (//))
import Data.Array.IArray (assocs, (!?))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Util (Coords, Grid, east, findAllIndices, north, readChar, readDenseGrid, south, west)

type TrailMap = Grid Int

type TrailCount = Grid Int

readInput :: ByteString -> TrailMap
readInput = fmap (\case '.' -> (-1); c -> readChar c) . readDenseGrid

trailCountFrom :: TrailMap -> Coords -> TrailCount
trailCountFrom g = go (fmap (const 0) g) (-1)
  where
    go trailCount lastHeight p
      | Just count <- g !? p,
        count == lastHeight + 1 =
          let trailCountNorth = go (trailCount // [(p, trailCount ! p + 1)]) (g ! p) (north p 1)
              trailCountEast = go trailCountNorth (g ! p) (east p 1)
              trailCountSouth = go trailCountEast (g ! p) (south p 1)
              trailCountWest = go trailCountSouth (g ! p) (west p 1)
           in trailCountWest
      | otherwise = trailCount

score :: TrailMap -> TrailCount -> Int
score trailMap trailCount =
  assocs trailCount
    & filter (\(pos, count) -> count > 0 && trailMap !? pos == Just 9)
    & length

part1 :: ByteString -> Int
part1 input =
  let trailMap = readInput input
   in findAllIndices (== 0) trailMap
        & map (trailCountFrom trailMap)
        & map (score trailMap)
        & sum

rating :: TrailMap -> TrailCount -> Int
rating trailMap trailCount =
  assocs trailCount
    & filter (\(pos, count) -> count > 0 && trailMap !? pos == Just 9)
    & map snd
    & sum

part2 :: ByteString -> Int
part2 input =
  let trailMap = readInput input
   in findAllIndices (== 0) trailMap
        & map (trailCountFrom trailMap)
        & map (rating trailMap)
        & sum
