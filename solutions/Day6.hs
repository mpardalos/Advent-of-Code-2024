module Day6 (part1, part2) where

import Data.Array.IArray (IArray, Ix, assocs, (!?), (//))
import Data.ByteString (ByteString)
import Data.List (find, unfoldr)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Safe (fromJustNote)
import Util (UGrid, readDenseGrid)
import Control.Parallel.Strategies (withStrategy, parListChunk, rseq)

findIdx :: (IArray a e, Ix i) => (e -> Bool) -> a i e -> Maybe (i, e)
findIdx p arr = find (p . snd) $ assocs arr

data Direction = N | E | S | W
  deriving (Eq, Ord, Show)

type Position = (Int, Int)

applyDirection :: Direction -> Position -> Position
applyDirection N (r, c) = (r - 1, c)
applyDirection E (r, c) = (r, c + 1)
applyDirection S (r, c) = (r + 1, c)
applyDirection W (r, c) = (r, c - 1)

turnRight :: Direction -> Direction
turnRight N = E
turnRight E = S
turnRight S = W
turnRight W = N

type State = (Position, Direction)

stepAll :: UGrid Char -> State -> [State]
stepAll g (pos, dir) = do
  case g !? applyDirection dir pos of
    Nothing -> []
    Just '#' -> [(pos, turnRight dir)]
    Just _ ->
      map (,dir)
        . takeWhile (\p -> g !? p `elem` [Just '.', Just '^'])
        $ iterate (applyDirection dir) pos

stepsFrom :: UGrid Char -> State -> [[State]]
stepsFrom grid st0 =
  [st0]
    : ( unfoldr
          ( \st -> do
              case stepAll grid st of
                [] -> Nothing
                states -> Just (states, last states)
          )
          st0
      )

spinePathFrom :: UGrid Char -> State -> [State]
spinePathFrom g st0 = map last $ stepsFrom g st0

fullPathFrom :: UGrid Char -> State -> [State]
fullPathFrom g st0 = concat $ stepsFrom g st0

part1 :: ByteString -> Int
part1 input =
  let grid = readDenseGrid input
      initialPos =
        fst
          . fromJustNote "Missing initial position"
          $ findIdx (== '^') grid
   in length . Set.fromList . map fst $ fullPathFrom grid (initialPos, N)

loopFrom :: UGrid Char -> State -> Bool
loopFrom g state = go Set.empty (spinePathFrom g state)
  where
    go _ [] = False
    go visited (x : xs) = Set.member x visited || go (Set.insert x visited) xs

addedObstructions :: UGrid Char -> [UGrid Char]
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
   in sum
        . withStrategy (parListChunk 1000 rseq)
        . map (\g -> if loopFrom g (initialPos, N) then 1 else 0)
        . addedObstructions
        $ grid
