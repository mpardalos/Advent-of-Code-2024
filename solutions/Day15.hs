module Day15 (part1, part2, interactive) where

import Control.Applicative (many)
import Control.Arrow (second)
import Control.Monad (void)
import Control.Monad.State
import Data.Attoparsec.ByteString.Char8 (anyChar, endOfInput, endOfLine, inClass, manyTill, satisfy, sepBy)
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.List (find, foldl')
import Data.Map (Map, (!), (!?))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import GHC.IO.Handle (BufferMode (NoBuffering), hSetBuffering)
import Safe (fromJustNote)
import System.IO (stdin)
import Util (Coords, east, north, parseOrError, south, west)

-- | <, >, ^, or v
type Instruction = Char

-- | @, O, #, [ or ]
type Object = Char

type GridMap = Map Coords Object

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
runInstruction instruction initCoords = runStateT (go initCoords)
  where
    go :: Coords -> StateT GridMap Maybe Coords
    go coords = do
      let nextCoords = case instruction of
            '^' -> north coords 1
            'v' -> south coords 1
            '>' -> east coords 1
            '<' -> west coords 1
            c -> error ("Invalid instruction: " ++ show c)

      grid <- get

      case grid !? nextCoords of
        Nothing -> do
          modify (Map.insert nextCoords (grid ! coords))
          modify (Map.delete coords)
        Just '#' -> fail "wall"
        Just c | c `elem` "@O[]" -> do
          _ <- go nextCoords
          modify (Map.insert nextCoords (grid ! coords))
          modify (Map.delete coords)
        Just c -> error ("Invalid object: " ++ show c)

      case grid ! coords of
        _ | instruction `notElem` "^v" -> pure ()
        '['
          | Just ']' <- grid !? east coords 1 ->
              void $ go (east coords 1)
        ']'
          | Just '[' <- grid !? west coords 1 ->
              void $ go (west coords 1)
        _withoutPair -> pure ()

      return nextCoords

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
  let (initCoordsPart1, gm, instructions) = readInput input
      initCoords = second (* 2) initCoordsPart1
   in gm
        & part2Expand
        & runInstructions instructions initCoords
        & snd
        & Map.filter (== '[')
        & Map.keys
        & map (\(row, col) -> 100 * row + col)
        & sum

interactive :: ByteString -> IO ()
interactive input = do
  hSetBuffering stdin NoBuffering
  go (second (* 2) initCoords) (part2Expand initGm)
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
        getChar <&> \case
          'k' -> '^'
          'j' -> 'v'
          'h' -> '<'
          'l' -> '>'
      runInstruction instruction coords gm
        & fromMaybe (coords, gm)
        & uncurry go
