module Day6 (part1, part2) where

import Data.Array.IArray (IArray, Ix, assocs, (!?), (//))
import Data.ByteString (ByteString)
import Data.List (find, unfoldr)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Safe (fromJustNote)
import Util (Grid, readDenseGrid)

findIdx :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> Maybe (i, e)
findIdx p arr = find (p . snd) $ assocs arr

data Direction = N | E | S | W
  deriving (Eq, Ord, Show)

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

pathFrom :: Grid Char -> Position -> Direction -> [(Position, Direction)]
pathFrom grid initialPos initialDir =
  (initialPos, initialDir)
    : unfoldr
      ( \st -> do
          st' <- step grid st
          return (st', st')
      )
      (initialPos, initialDir)

part1 :: ByteString -> Int
part1 input =
  let grid = readDenseGrid input
      initialPos =
        fst
          . fromJustNote "Missing initial position"
          $ findIdx (== '^') grid
   in length . Set.fromList . map fst $ pathFrom grid initialPos N

loopFrom :: Grid Char -> Position -> Direction -> Bool
loopFrom g p d = go Set.empty (pathFrom g p d)
  where
    go _ [] = False
    go visited (x : xs) = Set.member x visited || go (Set.insert x visited) xs

addedObstructions :: Grid Char -> [Grid Char]
addedObstructions g = mapMaybe obstructionAt (assocs g)
  where
    obstructionAt (pos, '.') = Just (g // [(pos, '#')])
    obstructionAt _ = Nothing

part2 :: ByteString -> Int
part2 input =
  let grid = readDenseGrid input
      initialPos =
        fst
          . fromJustNote "Missing initial position"
          $ findIdx (== '^') grid
   in length
        . filter (\g -> loopFrom g initialPos N)
        . addedObstructions
        $ grid
