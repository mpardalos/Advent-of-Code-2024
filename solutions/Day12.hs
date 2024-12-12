{-# LANGUAGE MultiWayIf #-}

module Day12 (part1, part2) where

import Control.Monad (unless)
import Control.Monad.ST (ST, runST)
import Data.Array.IArray (IArray (bounds), Ix (inRange), indices, (!), (!?))
import Data.Array.MArray (MArray (newArray), readArray, writeArray)
import Data.ByteString (ByteString)
import Data.STRef (STRef, modifySTRef', newSTRef, readSTRef)
import Util (Coords, STUGrid, UGrid, east, north, readDenseGrid, south, west)

type STVisited s = STUGrid s Bool

type GardenMap = UGrid Char

statsFrom :: forall s. GardenMap -> STVisited s -> Coords -> ST s (Int, Int, Int)
statsFrom g visited startCoords = do
  area <- newSTRef 0
  corners <- newSTRef 0
  perimeter <- newSTRef 0
  go (g ! startCoords) area corners perimeter startCoords
  (,,) <$> readSTRef area <*> readSTRef corners <*> readSTRef perimeter
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

    go :: Char -> STRef s Int -> STRef s Int -> STRef s Int -> Coords -> ST s ()
    go plant area corners perimeter coords
      | not (inRange (bounds g) coords) = modifySTRef' perimeter (1 +)
      | g ! coords /= plant = modifySTRef' perimeter (1 +)
      | otherwise = do
          alreadyVisited <- readArray visited coords
          unless alreadyVisited $ do
            modifySTRef' corners (cornersAt coords +)
            modifySTRef' area (1 +)
            writeArray visited coords True
            go plant area corners perimeter (north coords 1)
            go plant area corners perimeter (east coords 1)
            go plant area corners perimeter (south coords 1)
            go plant area corners perimeter (west coords 1)

fullFencePrice :: GardenMap -> Int
fullFencePrice g = runST $ do
  visited <- newArray (bounds g) False
  results <- mapM (statsFrom g visited) (indices g)
  return $ sum [area * perimeter | (area, _, perimeter) <- results]

part1 :: ByteString -> Int
part1 = fullFencePrice . readDenseGrid

discountedFencePrice :: GardenMap -> Int
discountedFencePrice g = runST $ do
  visited <- newArray (bounds g) False
  results <- mapM (statsFrom g visited) (indices g)
  return $ sum [area * corners | (area, corners, _) <- results]

part2 :: ByteString -> Int
part2 = discountedFencePrice . readDenseGrid
