module Day1 (part1, part2) where

import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.IntMultiSet qualified as IntMultiSet
import Data.List (sort)

readVerticalLists :: ByteString -> ([Int], [Int])
readVerticalLists input =
  BS.lines input
    & map BS.words
    & map (\[l, r] -> (l, r))
    & map (bimap (read . BS.unpack) (read . BS.unpack))
    & unzip

part1 :: ByteString -> Int
part1 input =
  readVerticalLists input
    & bimap sort sort
    & uncurry (zipWith ((abs .) . subtract))
    & sum

part2 :: ByteString -> Int
part2 input =
  let (l, r) = bimap sort sort $ readVerticalLists input
      multiset_right = IntMultiSet.fromList r
   in sum [n * IntMultiSet.occur n multiset_right | n <- l]
