{-# LANGUAGE OverloadedStrings #-}

module Day14 (part1, part2) where

import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8
  ( char,
    decimal,
    signed,
    string,
  )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.List (foldl', unfoldr)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import GHC.Stack (HasCallStack)
import Optics (makeFieldLabelsNoPrefix, view, (&))
import Text.Printf (printf)
import Util (expecting, linesOf, parseOrError)

data Robot = MkRobot
  { position :: (Int, Int),
    velocity :: (Int, Int)
  }
  deriving (Show)

makeFieldLabelsNoPrefix ''Robot

type Bounds = (Int, Int)

readInput :: ByteString -> [Robot]
readInput = parseOrError . linesOf $ do
  void $ string "p="
  px <- decimal
  void $ char ','
  py <- decimal
  void $ string " v="
  vx <- signed decimal
  void $ char ','
  vy <- signed decimal
  return MkRobot {position = (px, py), velocity = (vx, vy)}

stepRobot :: Bounds -> Robot -> Robot
stepRobot (width, height) MkRobot {position = (px, py), velocity = (vx, vy)} =
  MkRobot
    { position = ((px + vx) `mod` width, (py + vy) `mod` height),
      velocity = (vx, vy)
    }

countBy :: (Ord k) => (a -> k) -> [a] -> Map k Int
countBy f =
  foldl'
    (\m x -> Map.insertWith (+) (f x) 1 m)
    Map.empty

showPosition :: Bounds -> Robot -> String
showPosition (cols, rows) MkRobot {position = (x, y)} =
  unfoldr
    ( \(r, c) ->
        if
          | r == rows -> Nothing
          | c == cols -> Just ('\n', (r + 1, 0))
          | r == y && c == x -> Just ('1', (r, c + 1))
          | otherwise -> Just ('.', (r, c + 1))
    )
    (0, 0)

showPositions :: Bounds -> [Robot] -> String
showPositions (width, height) robots =
  let positions = countBy (view #position) robots
   in concat $
        unfoldr
          ( \(r, c) ->
              if
                | r == height -> Nothing
                | c == width -> Just ("\n", (r + 1, 0))
                | Just count <- Map.lookup (c, r) positions ->
                    Just (show count, (r, c + 1))
                | otherwise -> Just (".", (r, c + 1))
          )
          (0, 0)

data Quadrant = NW | NE | SW | SE
  deriving (Eq, Show, Ord)

quadrant :: Bounds -> (Int, Int) -> Maybe Quadrant
quadrant (width, height) (x, y) =
  case (compare x (width `div` 2), compare y (height `div` 2)) of
    (EQ, _) -> Nothing
    (_, EQ) -> Nothing
    (LT, LT) -> Just NW
    (LT, GT) -> Just SW
    (GT, LT) -> Just NE
    (GT, GT) -> Just SE

countByQuadrant :: Bounds -> [Robot] -> Map (Maybe Quadrant) Int
countByQuadrant bounds = countBy (quadrant bounds . view #position)

part1 :: (HasCallStack) => ByteString -> Int
part1 input =
  readInput input
    & map ((!! 100) . iterate (stepRobot gridSize))
    & countByQuadrant gridSize
    & Map.filterWithKey (\q _ -> isJust q)
    & expecting ((< 5) . length)
    & product
  where
    gridSize = (101, 103)

part2 :: ByteString -> ()
part2 _ = ()
