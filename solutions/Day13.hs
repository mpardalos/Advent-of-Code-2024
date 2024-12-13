{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}


module Day13 (part1, part2) where

import Control.Applicative.Combinators (some)
import Control.Monad (guard, void)
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine, string, endOfInput)
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import Util (parseOrError)
import Optics

data ClawMachine = MkClawMachine
  { aX :: Int,
    aY :: Int,
    bX :: Int,
    bY :: Int,
    prizeX :: Int,
    prizeY :: Int
  }
  deriving Show

makeFieldLabelsNoPrefix ''ClawMachine

readInput :: ByteString -> [ClawMachine]
readInput = parseOrError $ (<* endOfInput) $ some $ do
  _ <- string "Button A: X+"
  aX <- decimal
  _ <- string ", Y+"
  aY <- decimal
  endOfLine
  _ <- string "Button B: X+"
  bX <- decimal
  _ <- string ", Y+"
  bY <- decimal
  endOfLine
  _ <- string "Prize: X="
  prizeX <- decimal
  _ <- string ", Y="
  prizeY <- decimal
  void $ some endOfLine
  return MkClawMachine {..}

solveClawMachine :: ClawMachine -> Maybe (Int, Int)
solveClawMachine MkClawMachine {..} = do
  let det = aX * bY - aY * bX
  let a1 = bY * prizeX - bX * prizeY
  let b1 = aX * prizeY - aY * prizeX
  guard (det /= 0)
  guard (a1 `mod` det == 0)
  guard (b1 `mod` det == 0)
  let aCount = a1 `div` det
  guard (aCount >= 0)
  let bCount = b1 `div` det
  guard (bCount >= 0)
  return (aCount, bCount)


solutionCost :: (Int, Int) -> Int
solutionCost (a, b) = 3 * a + b

part1 :: ByteString -> Int
part1 =
  sum . map solutionCost . mapMaybe solveClawMachine . readInput

part2Correction :: ClawMachine -> ClawMachine
part2Correction = over #prizeX (+10000000000000) . over #prizeY (+10000000000000)

part2 :: ByteString -> Int
part2 =
  sum . map solutionCost . mapMaybe solveClawMachine . map part2Correction . readInput
