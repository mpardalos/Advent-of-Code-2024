{-# LANGUAGE NoMonomorphismRestriction #-}

module Day16 (part1, part2) where

import Control.Monad (guard)
import Data.Array.IArray (indices)
import Data.Array.IArray qualified as Array
import Data.ByteString (ByteString)
import Data.Foldable (foldl')
import Data.Function ((&))
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Semigroup (Arg (Arg))
import Util (Coords, Grid, east, findAllIndices, north, readDenseGrid, south, west)

type Maze = Grid Char

readInput :: ByteString -> (Coords, Coords, Maze)
readInput input =
  let g = readDenseGrid input
   in ( head $ findAllIndices (== 'S') g,
        head $ findAllIndices (== 'E') g,
        g
      )

-- | Nothing means infinity
data Distance = Inf | Distance {-# UNPACK #-} !Int
  deriving (Eq)

instance Show Distance where
  show Inf = "_"
  show (Distance d) = show d

instance Ord Distance where
  compare Inf Inf = EQ
  compare (Distance _) Inf = LT
  compare Inf (Distance _) = GT
  compare (Distance l) (Distance r) = compare l r

fromDistance :: Distance -> Int
fromDistance Inf = error "Infinite distance"
fromDistance (Distance d) = d

data Direction = N | E | S | W
  deriving (Eq, Ord, Show)

type State = (Coords, Direction)

dijkstraDistances ::
  forall s.
  (Eq s, Ord s) =>
  (s -> [(Int, s)]) ->
  Heap s ->
  Map s Distance ->
  Map s Distance
dijkstraDistances neighbours q0 distances = case Heap.viewMin q0 of
  Nothing -> distances
  Just (nextCoords, q') ->
    let newDistances = foldl' (\m (s, d) -> Map.insert s d m) distances $ do
          (cost, n) <- neighbours nextCoords
          let alt = case distances ! nextCoords of
                Inf -> Inf
                Distance d -> Distance (d + cost)
          guard (alt /= Inf)
          guard (alt < distances ! n)
          [(n, alt)]
     in dijkstraDistances neighbours q' newDistances

distancesFrom :: State -> Maze -> Map State Distance
distancesFrom initState m =
  dijkstraDistances neighbours (Heap.fromList allStates) initDistances
  where
    neighbours =
      filter (\(_, (c, _)) -> (m Array.!? c) `elem` [Just 'S', Just 'E', Just '.'])
        . rawNeighbours
    rawNeighbours (c, N) =
      [ (1, (north c 1, N)),
        (1000, (c, E)),
        (1000, (c, W))
      ]
    rawNeighbours (c, E) =
      [ (1, (east c 1, E)),
        (1000, (c, N)),
        (1000, (c, S))
      ]
    rawNeighbours (c, S) =
      [ (1, (south c 1, S)),
        (1000, (c, W)),
        (1000, (c, E))
      ]
    rawNeighbours (c, W) =
      [ (1, (west c 1, W)),
        (1000, (c, S)),
        (1000, (c, N))
      ]
    allStates =
      concat [[(c, N), (c, E), (c, S), (c, W)] | c <- indices m]
    initDistances =
      Map.fromList $
        map
          (\s -> if s == initState then (s, Distance 0) else (s, Inf))
          allStates

part1 :: ByteString -> Int
part1 input =
  let (start, end, grid) = readInput input
      distances = distancesFrom (start, E) grid
   in minimum [distances ! (end, N), distances ! (end, E), distances ! (end, S), distances ! (end, W)]
        & fromDistance

part2 :: ByteString -> ()
part2 _ = ()
