{-# LANGUAGE LambdaCase #-}

module Day10 (part1, part2) where

import Control.Monad.ST (ST)
import Data.Array.IArray (amap, assocs, (!), (!?))
import Data.Array.MArray (modifyArray')
import Data.Array.Unboxed (UArray)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Util (Coords, STUGrid, UGrid, east, findAllIndices, north, readChar, readDenseGrid, south, west, withMutableArray)

type TrailMap = UGrid Int

type TrailCount = UGrid Int

type STUTrailCount s = STUGrid s Int

readInput :: ByteString -> TrailMap
readInput = amap (\case '.' -> (-1); c -> readChar c) . readDenseGrid @UArray

trailCountFrom :: TrailMap -> Coords -> TrailCount
trailCountFrom g startPos =
  withMutableArray
    (go (-1) startPos)
    (amap (const 0) g)
  where
    go :: forall s. Int -> Coords -> STUTrailCount s -> ST s ()
    go lastHeight p trailCount
      | Just height <- g !? p,
        height == lastHeight + 1 = do
          modifyArray' trailCount p (1 +)
          go (g ! p) (north p 1) trailCount
          go (g ! p) (east p 1) trailCount
          go (g ! p) (south p 1) trailCount
          go (g ! p) (west p 1) trailCount
      | otherwise = return ()

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
