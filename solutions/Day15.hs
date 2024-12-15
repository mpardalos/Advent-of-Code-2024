module Day15 (part1, part2, interactive) where

import Control.Applicative (many)
import Data.Attoparsec.ByteString.Char8 (anyChar, endOfInput, endOfLine, inClass, manyTill, satisfy, sepBy)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find, foldl')
import Data.Map (Map, (!), (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Safe (fromJustNote)
import System.IO (hClose, stdin)
import Util (Coords, east, north, parseOrError, south, west)

-- | <, >, ^, or v
type Instruction = Char

-- | O or #
type Object = Char

type GridMap = Map Coords Object

-- let mSiblingCoords = case c of
--       '[' -> let (row, col) = coords in Just (row, col + 1)
--       ']' -> let (row, col) = coords in Just (row, col - 1)
--       _ -> Nothing
-- (coords', gm') <- runInstruction instruction nextCoords gm
-- (_, gm'') <- case mSiblingCoords of
--   Just siblingCoords
--     | instruction `elem` "v^" ->
--         runInstruction instruction siblingCoords gm'
--   _ignore ->
--     pure (coords', gm')
-- let newNewMap =
--       gm''
--         & Map.insert nextCoords c
--         & Map.delete coords
-- Just (nextCoords, newNewMap)

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
  Just c | c `elem` "@O" -> do
    (_, gm') <- runInstruction instruction nextCoords gm
    let gm'' = gm' & Map.insert nextCoords (gm ! coords) & Map.delete coords
    Just (nextCoords, gm'')
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

showGrid :: GridMap -> String
showGrid gm =
  unlines
    [ [ showObject (gm !? (r, c))
        | c <- [0 .. maxCol]
      ]
      | r <- [0 .. maxRow]
    ]
  where
    showObject Nothing = '.'
    showObject (Just c) = c

    maxCol = maximum . map snd . Map.keys $ gm
    maxRow = maximum . map fst . Map.keys $ gm

part1 :: ByteString -> Int
part1 input =
  let (robot, gm, instructions) = readInput input
   in gm
        & runInstructions instructions robot
        & snd
        & Map.filter (== 'O')
        & Map.keys
        & map (\(row, col) -> 100 * row + col)
        & sum

part2Expand :: GridMap -> GridMap
part2Expand =
  Map.fromList
    . concatMap
      ( \((row, col), o) -> case o of
          '#' -> [((row, 2 * col), '#'), ((row, 2 * col + 1), '#')]
          'O' -> [((row, 2 * col), '['), ((row, 2 * col + 1), ']')]
          '@' -> [((row, 2 * col), '@')]
          _ -> error ("Invalid object: " ++ show o)
      )
    . Map.toList

part2 :: ByteString -> Int
part2 input =
  let (robot, gm, instructions) = readInput input
   in gm
        & part2Expand
        & runInstructions instructions robot
        & snd
        & Map.filter (== 'O')
        & Map.keys
        & map (\(row, col) -> 100 * row + col)
        & sum

interactive :: ByteString -> IO ()
interactive input = do
  hSetBuffering stdin NoBuffering
  go initCoords initGm
  where
    (initCoords, initGm, _instructions) = readInput input

    go :: Coords -> GridMap -> IO ()
    go coords gm = do
      putStrLn
        ( map
            ( \case
                '.' -> ' '
                c -> c
            )
            $ showGrid gm
        )
      instruction <-
        untilSuccess $
          getChar <&> \case
            'k' -> Just '^'
            'j' -> Just 'v'
            'h' -> Just '<'
            'l' -> Just '>'
            _ -> Nothing
      runInstruction instruction coords gm
        & fromMaybe (coords, gm)
        & uncurry go

untilSuccess :: IO (Maybe a) -> IO a
untilSuccess f =
  f >>= \case
    Just x -> pure x
    Nothing -> untilSuccess f
