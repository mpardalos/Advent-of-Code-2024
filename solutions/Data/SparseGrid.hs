{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.SparseGrid where

import Control.Applicative (Applicative (..))
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, endOfLine, many1, sepBy, takeTill)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Map.Merge.Lazy qualified as Map
import GHC.Generics (Generic)
import Optics
import Safe (fromJustDef)

type Position = (Int, Int)

data SparseGrid a = SparseGrid
  { width :: !Int,
    height :: !Int,
    content :: !(Map Position a),
    emptyValue :: a
  }
  deriving (Generic, Functor, Foldable)

set :: Position -> a -> SparseGrid a -> SparseGrid a
set p x g =
  if inBounds g p
    then over #content (Map.insert p x) g
    else error "Data.SparseGrid.set: out of bounds"

get :: Position -> SparseGrid a -> a
get p g =
  if inBounds g p
    then fromJustDef g.emptyValue $ Map.lookup p $ g.content
    else error "Data.SparseGrid.get: out of bounds"

update :: Position -> (Maybe a -> Maybe a) -> SparseGrid a -> SparseGrid a
update p f g =
  if inBounds g p
    then over #content (Map.alter f p) g
    else error "Data.SparseGrid.update: out of bounds"

allPositions :: SparseGrid a -> [Position]
allPositions g =
  [ (r, c)
    | r <- [0 .. g.height - 1],
      c <- [0 .. g.width - 1]
  ]

getRow :: Int -> SparseGrid a -> [a]
getRow row g =
  [ get (row, col) g
    | col <- [0 .. g.height - 1]
  ]

rows :: SparseGrid a -> [[a]]
rows g = [getRow r g | r <- [0 .. g.height - 1]]

getColumn :: Int -> SparseGrid a -> [a]
getColumn col g =
  [ get (row, col) g
    | row <- [0 .. g.width - 1]
  ]

columns :: SparseGrid a -> [[a]]
columns g = [getColumn c g | c <- [0 .. g.width - 1]]

inBounds :: SparseGrid a -> Position -> Bool
inBounds g (row, col) =
  row >= 0
    && col >= 0
    && row < g.height
    && col < g.width

sparseGridP :: Char -> Parser (SparseGrid Char)
sparseGridP emptyChar = do
  gridLines <- (many1 anyChar `sepBy` endOfLine)
  return
    SparseGrid
      { height = length gridLines,
        width = length (head gridLines),
        emptyValue = emptyChar,
        content =
          Map.fromList
            [ ((row, col), c)
              | (row, line) <- zip [0 ..] gridLines,
                (col, c) <- zip [0 ..] line
            ]
      }

parseSparseGrid :: Char -> ByteString -> SparseGrid Char
parseSparseGrid emptyChar input =
  let content = go Map.empty (0, 0) (BS.unpack input)
      height = length (BS.lines input)
      width = BS.length (head (BS.lines input))
   in SparseGrid {width, height, content, emptyValue = emptyChar}
  where
    go m _ [] = m
    go m (row, col) (c : cs) = case c of
      '\n' -> go m (row + 1, 0) cs
      _ | c == emptyChar -> go m (row, col + 1) cs
      _ -> go (Map.insert (row, col) c m) (row, col + 1) cs

displayGrid :: SparseGrid Char -> String
displayGrid = unlines . rows

addWall :: a -> SparseGrid a -> SparseGrid a
addWall x g =
  let width = g.width + 2
      height = g.height + 2
      originalContent = Map.mapKeys (\(r, c) -> (r + 1, c + 1)) g.content
      northWall = Map.fromList [((0, c), x) | c <- [0 .. width - 1]]
      eastWall = Map.fromList [((r, width - 1), x) | r <- [0 .. height - 1]]
      southWall = Map.fromList [((height - 1, c), x) | c <- [0 .. width - 1]]
      westWall = Map.fromList [((r, 0), x) | r <- [0 .. height - 1]]
      content = Map.unions [originalContent, northWall, eastWall, southWall, westWall]
   in g {width, height, content}

instance Applicative SparseGrid where
  pure x =
    SparseGrid
      { height = 0,
        width = 0,
        emptyValue = x,
        content = Map.empty
      }
  liftA2 f g1 g2 =
    SparseGrid
      { height = max g1.height g2.height,
        width = max g1.height g2.height,
        emptyValue = f g1.emptyValue g2.emptyValue,
        content =
          Map.merge
            (Map.mapMissing (\_ x -> x `f` g2.emptyValue))
            (Map.mapMissing (\_ x -> g1.emptyValue `f` x))
            (Map.zipWithMatched (\_ x y -> x `f` y))
            g1.content
            g2.content
      }

makeFieldLabelsNoPrefix ''SparseGrid
