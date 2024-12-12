{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Day12 (part1, part2) where

import Control.Monad (unless)
import Control.Monad.ST (ST, runST)
import Data.Array.IArray (IArray (bounds), Ix (inRange), indices, (!), (!?))
import Data.Array.MArray (MArray (newArray), readArray, writeArray)
import Data.ByteString (ByteString)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Optics (makeFieldLabelsNoPrefix, (%~))
import Util (Coords, STUGrid, UGrid, east, north, readDenseGrid, south, west)

type STVisited s = STUGrid s Bool

type GardenMap = UGrid Char

data Stats = MkStats
  { area :: Int,
    corners :: Int,
    perimeter :: Int
  }

makeFieldLabelsNoPrefix ''Stats

statsFrom :: forall s. GardenMap -> STVisited s -> Coords -> ST s Stats
statsFrom g visited startCoords = do
  stats <- newSTRef (MkStats 0 0 0)
  go (g ! startCoords) stats startCoords
  readSTRef stats
  where
    cornersAt :: Coords -> Int
    cornersAt coords =
      let c = g !? coords
          n = g !? north coords 1
          ne = g !? north (east coords 1) 1
          e = g !? east coords 1
          se = g !? south (east coords 1) 1
          s = g !? south coords 1
          sw = g !? south (west coords 1) 1
          w = g !? west coords 1
          nw = g !? north (west coords 1) 1
       in length . filter id $
            [ n /= c && e /= c,
              n /= c && w /= c,
              s /= c && w /= c,
              s /= c && e /= c,
              n == c && e == c && ne /= c,
              n == c && w == c && nw /= c,
              s == c && w == c && sw /= c,
              s == c && e == c && se /= c
            ]

    go :: Char -> STRef s Stats -> Coords -> ST s ()
    go plant stats coords
      -- If we step out of our area, then we have found an edge
      | not (inRange (bounds g) coords) = modifySTRef' stats (#perimeter %~ (+ 1))
      | g ! coords /= plant = modifySTRef' stats (#perimeter %~ (+ 1))
      | otherwise = do
          alreadyVisited <- readArray visited coords
          unless alreadyVisited $ do
            modifySTRef' stats (#corners %~ (+ cornersAt coords))
            modifySTRef' stats (#area %~ (+ 1))
            writeArray visited coords True
            go plant stats (north coords 1)
            go plant stats (east coords 1)
            go plant stats (south coords 1)
            go plant stats (west coords 1)

fullFencePrice :: GardenMap -> Int
fullFencePrice g = runST $ do
  visited <- newArray (bounds g) False
  stats <- mapM (statsFrom g visited) (indices g)
  return $ sum [s.area * s.perimeter | s <- stats]

part1 :: ByteString -> Int
part1 = fullFencePrice . readDenseGrid

discountedFencePrice :: GardenMap -> Int
discountedFencePrice g = runST $ do
  visited <- newArray (bounds g) False
  stats <- mapM (statsFrom g visited) (indices g)
  return $ sum [s.area * s.corners | s <- stats]

part2 :: ByteString -> Int
part2 = discountedFencePrice . readDenseGrid
