module Day22 (part1, part2) where

import Control.Applicative.Combinators (sepBy)
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine)
import Data.Bits (Bits (xor, (.&.)), (.<<.), (.>>.))
import Data.ByteString (ByteString)
import Data.Function ((&))
import Data.Vector qualified as V
import GHC.Exts (IsList (Item, fromList))
import Util (parseOrError)

readInput :: (IsList l, Integral (Item l)) => ByteString -> l
readInput = fromList . parseOrError (decimal `sepBy` endOfLine)

mix :: Int -> Int -> Int
mix = xor

prune :: Int -> Int
prune = (.&. (16777216 - 1))

next :: Int -> Int
next secret =
  secret
    & (\n -> prune $ mix n (n .<<. 6))
    & (\n -> mix n (n .>>. 5))
    & (\n -> prune $ mix n (n .<<. 11))

part1 :: ByteString -> Int
part1 input =
  readInput input
    & V.map ((!! 2000) . iterate next)
    & sum

part2 :: ByteString -> ()
part2 _ = ()
