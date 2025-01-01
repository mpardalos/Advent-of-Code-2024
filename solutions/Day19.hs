{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Day19 (part1, part2) where

import Control.Applicative.Combinators (sepBy, some)
import Control.Monad.Extra (anyM, filterM)
import Data.Attoparsec.ByteString.Char8 (endOfLine, letter_ascii)
import Data.ByteString (ByteString)
import Data.List.Extra (stripPrefix)
import Util (Memo, memo, parseOrError, runMemoM)

type Towel = String

type Design = String

readInput :: ByteString -> ([Towel], [Design])
readInput = parseOrError $ do
  available <- some letter_ascii `sepBy` ", "
  endOfLine
  endOfLine
  desired <- some letter_ascii `sepBy` endOfLine
  return (available, desired)

canCreate :: [Towel] -> Design -> Memo s Design Bool
canCreate towels = memo $ \case
  "" -> return True
  design ->
    anyM
      ( \towel ->
          case stripPrefix towel design of
            Nothing -> return False
            Just rest -> canCreate towels rest
      )
      towels

arrangements :: [Towel] -> Design -> Memo s Design Int
arrangements towels = memo $ \case
  "" -> return 1
  design ->
    sum
      <$> mapM
        ( \towel ->
            case stripPrefix towel design of
              Nothing -> return 0
              Just rest -> arrangements towels rest
        )
        towels

part1 :: ByteString -> Int
part1 input =
  let (available, desired) = readInput input
   in length $ runMemoM (filterM (canCreate available) desired)

part2 :: ByteString -> Int
part2 input =
  let (available, desired) = readInput input
   in sum $ runMemoM (mapM (arrangements available) desired)
