module Day15 (part1, part2) where

import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8 (anyChar, endOfInput, endOfLine, inClass, manyTill, satisfy, sepBy)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.List (find, foldl')
import Data.Map (Map, (!), (!?))
import Data.Map qualified as Map
import Data.Vector (Vector)
import Data.Vector qualified as V
import Safe (fromJustNote)
import Util (Coords, east, north, parseOrError, south, west)

-- | <, >, ^, or v
type Instruction = Char

-- | O or #
type Object = Char

type GridMap = Map Coords Object

data State = MkState !Coords !GridMap

readInput :: ByteString -> (Coords, Map Coords Object, [Instruction])
readInput = parseOrError $ do
  gridList <- many (satisfy (inClass "#.O@")) `sepBy` endOfLine

  let gridMapWithRobot =
        gridList
          & map (zip [0 ..])
          & zipWith (\row -> map (\(col, c) -> ((row, col), c))) [0 ..]
          & concat
          & Map.fromList
          & Map.filter (/= '.')

  instructions <- filter (/= '\n') <$> manyTill anyChar endOfInput

  let robot =
        Map.toList gridMapWithRobot
          & find ((== '@') . snd)
          & fromJustNote "Missing robot"
          & fst

  return (robot, gridMapWithRobot, instructions)

runInstruction :: Instruction -> Coords -> GridMap -> Maybe (Coords, GridMap)
runInstruction instruction coords gm = case gm !? nextCoords of
  Nothing ->
    let newMap = gm & Map.insert nextCoords (gm ! coords) & Map.delete coords
     in Just (nextCoords, newMap)
  Just '#' -> Nothing
  Just c | c `elem` "@O" ->
    case runInstruction instruction nextCoords gm of
      Nothing -> Nothing
      Just (_, newMap) ->
        let newNewMap = newMap & Map.insert nextCoords (gm ! coords) & Map.delete coords
         in Just (nextCoords, newNewMap)
  Just c -> error ("Invalid object: " ++ show c)
  where
    nextCoords = case instruction of
      '^' -> north coords 1
      'v' -> south coords 1
      '>' -> east coords 1
      '<' -> west coords 1
      c -> error ("Invalid instruction: " ++ show c)

runInstructions :: [Instruction] -> Coords -> GridMap -> (Coords, GridMap)
runInstructions instructions initCoords initGm =
  foldl'
    ( \(coords, gm) instruction -> case runInstruction instruction coords gm of
        Just (coords', gm') -> (coords', gm')
        Nothing -> (coords, gm)
    )
    (initCoords, initGm)
    instructions

showGrid :: (Int, Int) -> GridMap -> String
showGrid (rows, cols) gm =
  unlines
    [ [ showObject (gm !? (r, c))
        | c <- [0 .. cols - 1]
      ]
      | r <- [0 .. rows - 1]
    ]
  where
    showObject Nothing = '.'
    showObject (Just c) = c

part1 :: ByteString -> Int
part1 input =
  let (robot, gm, instructions) = readInput input
      (_endCoords, finalGm) = runInstructions instructions robot gm
   in finalGm
        & Map.filter (== 'O')
        & Map.keys
        & map (\(row, col) -> 100 * row + col)
        & sum

part2 :: ByteString -> ()
part2 _ = ()
