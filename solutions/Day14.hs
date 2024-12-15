{-# LANGUAGE OverloadedStrings #-}

module Day14 (part1, part2, part2Vis) where

import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
  ( char,
    decimal,
    signed,
    string,
  )
import Data.ByteString (ByteString)
import Data.List (minimumBy, unfoldr)
import Data.Map qualified as Map
import Data.Ord (comparing)
import Data.Vector (Vector)
import GHC.IsList (IsList (Item, fromList))
import GHC.List (iterate')
import Optics (makeFieldLabelsNoPrefix, view, (&))
import Util (averageDistanceFromCenter, countBy, expecting, linesOf, parseOrError)

data Robot = MkRobot
  { position :: (Int, Int),
    velocity :: (Int, Int)
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''Robot

type Bounds = (Int, Int)

readInput :: (IsList l, Item l ~ Robot) => ByteString -> l
readInput = parseOrError . fmap fromList . linesOf $ do
  void $ string "p="
  px <- decimal
  void $ char ','
  py <- decimal
  void $ string " v="
  vx <- signed decimal
  void $ char ','
  vy <- signed decimal
  return MkRobot {position = (px, py), velocity = (vx, vy)}

stepRobot :: Int -> Bounds -> Robot -> Robot
stepRobot n (width, height) MkRobot {position = (px, py), velocity = (vx, vy)} =
  MkRobot
    { position = ((px + n * vx) `mod` width, (py + n * vy) `mod` height),
      velocity = (vx, vy)
    }

showPositions :: (Foldable t) => Bounds -> t Robot -> String
showPositions (width, height) robots =
  let positions = countBy (view #position) robots
   in concat $
        unfoldr
          ( \(r, c) ->
              if
                | r == height -> Nothing
                | c == width -> Just ("\n", (r + 1, 0))
                | Just _ <- Map.lookup (c, r) positions ->
                    Just ("#", (r, c + 1))
                | otherwise -> Just (" ", (r, c + 1))
          )
          (0, 0)

part1 :: ByteString -> Int
part1 input =
  readInput @[Robot] input
    & map (stepRobot 100 gridSize)
    & countBy
      ( \MkRobot {position = (x, y)} ->
          ( compare x (fst gridSize `div` 2),
            compare y (snd gridSize `div` 2)
          )
      )
    & Map.filterWithKey (\(xside, yside) _ -> xside /= EQ && yside /= EQ)
    & expecting ((< 5) . length)
    & product
  where
    gridSize = (101, 103)

part2Vis :: ByteString -> [String]
part2Vis =
  map (showPositions gridSize)
    . iterate (map (stepRobot 1 gridSize))
    . readInput
  where
    gridSize = (101, 103)

part2 :: ByteString -> Int
part2 input =
  readInput @(Vector Robot) input
    & iterate' (fmap (stepRobot 1 gridSize))
    & take 10000
    & zip [0 ..]
    & minimumBy
      ( comparing
          ( averageDistanceFromCenter
              . fmap (view #position)
              . snd
          )
      )
    & fst
  where
    gridSize = (101, 103)
