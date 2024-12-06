module Day6 (part1, part2) where

import Data.Array.IArray (IArray, Ix, assocs, (!?))
import Data.ByteString (ByteString)
import Data.List (find, unfoldr)
import Data.Set qualified as Set
import Safe (fromJustNote)
import Util (Grid, readDenseGrid)

findIdx :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> Maybe (i, e)
findIdx p arr = find (p . snd) $ assocs arr

data Direction = N | E | S | W
  deriving (Eq, Show)

type Position = (Int, Int)

applyDirection :: Position -> Direction -> Position
applyDirection (r, c) N = (r - 1, c)
applyDirection (r, c) E = (r, c + 1)
applyDirection (r, c) S = (r + 1, c)
applyDirection (r, c) W = (r, c - 1)

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

step :: Grid Char -> (Position, Direction) -> Maybe (Position, Direction)
step g (pos, dir) = do
  let nextPos = applyDirection pos dir
  facingCell <- g !? nextPos
  return $
    if facingCell == '#'
      then (pos, turnRight dir)
      else (nextPos, dir)

pathFrom :: Grid Char -> Position -> Direction -> [Position]
pathFrom grid initialPos initialDir =
  initialPos
    : unfoldr
      ( \(pos, dir) -> do
          (pos', dir') <- step grid (pos, dir)
          return (pos', (pos', dir'))
      )
      (initialPos, initialDir)

part1 :: ByteString -> Int
part1 input =
  let grid = readDenseGrid input
      initialPos =
        fst
          . fromJustNote "Missing initial position"
          $ findIdx (== '^') grid
   in length . Set.fromList $ pathFrom grid initialPos N

part2 :: ByteString -> ()
part2 _ = ()
