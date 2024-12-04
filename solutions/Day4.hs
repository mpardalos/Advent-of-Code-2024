module Day4 (part1, part2) where

import Control.Monad (guard)
import Data.Array (Array, bounds, listArray)
import Data.Array.IArray ((!?))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (isJust)

type Grid c = Array (Int, Int) c

xmasAtPos :: Grid Char -> (Int, Int) -> Int
xmasAtPos g (row, col) =
  let horizontal = traverse (g !?) [(row, col + i) | i <- [0 .. 3]]
      vertical = traverse (g !?) [(row + i, col) | i <- [0 .. 3]]
      diagonalRight = traverse (g !?) [(row + i, col + i) | i <- [0 .. 3]]
      diagonalLeft = traverse (g !?) [(row + i, col - i) | i <- [0 .. 3]]
   in length . filter id $
        [ horizontal `elem` [Just "XMAS", Just "SAMX"],
          vertical `elem` [Just "XMAS", Just "SAMX"],
          diagonalLeft `elem` [Just "XMAS", Just "SAMX"],
          diagonalRight `elem` [Just "XMAS", Just "SAMX"]
        ]

parseDenseGrid :: ByteString -> Grid Char
parseDenseGrid input =
  let chars :: [Char] = concatMap BS.unpack (BS.lines input)
      columns = BS.length (head (BS.lines input))
      rows = length (BS.lines input)
   in listArray ((0, 0), (rows - 1, columns - 1)) chars

part1 :: ByteString -> Int
part1 input =
  let grid = parseDenseGrid input
      ((0, 0), (maxRow, maxCol)) = bounds grid
      xmasPositions =
        [ xmasAtPos grid (row, col)
          | row <- [0 .. maxRow],
            col <- [0 .. maxCol]
        ]
   in sum xmasPositions

masAtPos :: Grid Char -> (Int, Int) -> Bool
masAtPos g (row, col) = isJust $ do
  center <- g !? (row + 1, col + 1)
  guard (center == 'A')

  let positions =
        [ (row + rowDiff, col + colDiff)
          | (rowDiff, colDiff) <- [(0, 0), (2, 2), (2, 0), (0, 2)]
        ]
  chars <- traverse (g !?) positions

  guard $
    chars
      `elem` [ "MSMS",
               "MSSM",
               "SMMS",
               "SMSM"
             ]

part2 :: ByteString -> Int
part2 input =
  let grid = parseDenseGrid input
      ((0, 0), (maxRow, maxCol)) = bounds grid
      xmasPositions =
        [ masAtPos grid (row, col)
          | row <- [0 .. maxRow],
            col <- [0 .. maxCol]
        ]
   in length . filter id $ xmasPositions
