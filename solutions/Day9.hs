{-# LANGUAGE OverloadedStrings #-}

module Day9 (part1, part2) where

import Data.Bifunctor (Bifunctor (second))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.List (unfoldr)
import Data.Tuple.Extra (dupe)
import Util (expectingNote, pairwiseSeparate)

type FileID = Int

data Block
  = Full !FileID
  | Empty
  deriving (Eq, Ord, Show)

readInput :: ByteString -> [Block]
readInput input =
  let nums :: [(FileID, (Int, Int))] =
        BS.strip input
          & BS.unpack
          & map (read . pure)
          & expectingNote "input to be odd" (odd . length)
          & (++ [0])
          & pairwiseSeparate
          & zip [0 ..]
   in concat
        [ replicate fileBlocks (Full fileID)
            ++ replicate emptyBlocks Empty
          | (fileID, (fileBlocks, emptyBlocks)) <- nums
        ]

checksum :: [Block] -> Int
checksum = go . zip [0 ..]
  where
    go [] = 0
    go ((pos, Full n) : xs) = pos * n + go xs
    go ((_, Empty) : _) = error "Empty in checksum"

placeEarliest :: FileID -> [Block] -> Maybe [Block]
placeEarliest fileID [] = Nothing
placeEarliest fileID (Full _ : xs) = placeEarliest fileID xs
placeEarliest fileID (Empty : xs) = Just (Full fileID : xs)

withoutLastFile :: [Block] -> (FileID, [Block])
withoutLastFile [] = error "No files in disk"
withoutLastFile (Full f : fs) | all (== Empty) fs = (f, [])
withoutLastFile (f : fs) = second (f :) $ withoutLastFile fs

repositionLast :: [Block] -> Maybe [Block]
repositionLast blocks =
  let (lastFile, blocks') = withoutLastFile blocks
   in placeEarliest lastFile blocks'

part1 :: ByteString -> Int
part1 input =
  readInput input
    & unfoldr (fmap dupe . repositionLast)
    & last
    & checksum

part2 :: ByteString -> ()
part2 _ = ()
