{-# LANGUAGE OverloadedStrings #-}

module Day9 (part1, part2) where

import Control.Monad.ST (ST)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
import Util (expectingNote, pairwiseSeparate)

type FileID = Int

data Block
  = Full !FileID
  | Empty
  deriving (Eq, Ord)

instance Show Block where
  show Empty = "."
  show (Full n) = show n

readInput :: ByteString -> Vector (Int, Block)
readInput input =
  let nums :: [(FileID, (Int, Int))] =
        BS.strip input
          & BS.unpack
          & map (read . pure)
          & expectingNote "input to be odd" (odd . length)
          & (++ [0])
          & pairwiseSeparate
          & zip [0 ..]
   in V.fromList $
        concat
          [ [(fileBlocks, Full fileID), (emptyBlocks, Empty)]
            | (fileID, (fileBlocks, emptyBlocks)) <- nums
          ]

defrag :: Vector Block -> Vector Block
defrag = V.modify (\vec -> go vec 0 (MV.length vec - 1))
  where
    go :: MVector s Block -> Int -> Int -> ST s ()
    go vec low high
      | low >= high = return ()
      | otherwise = do
          atLow <- MV.read vec low
          atHigh <- MV.read vec high
          case (atLow, atHigh) of
            (Empty, Full _) -> do
              MV.swap vec low high
              go vec (low + 1) (high - 1)
            (Full _, _) -> go vec (low + 1) high
            (_, Empty) -> go vec low (high - 1)

checksum :: (Foldable t) => t Block -> Int
checksum = go . zip [0 ..] . toList
  where
    go [] = 0
    go ((pos, Full n) : xs) = pos * n + go xs
    go ((_, Empty) : _) = 0

part1 :: ByteString -> Int
part1 input =
  readInput input
    & V.concatMap (uncurry V.replicate)
    & defrag
    & checksum

part2 :: ByteString -> ()
part2 _ = ()
