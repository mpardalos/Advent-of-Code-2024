{-# LANGUAGE OverloadedStrings #-}

module Day3 (part1, part2) where

import Control.Applicative (Alternative (many, some), (<|>))
import Control.Applicative.Combinators (manyTill_)
import Data.Attoparsec.ByteString.Char8 (anyChar, digit, string, try)
import Data.ByteString (ByteString)
import Util (parseOrError)

data Instruction
  = Mul !Int !Int
  | Do
  | Dont
  deriving (Show)

findInstructions :: ByteString -> [Instruction]
findInstructions = parseOrError (many $ findNext (mulInstr <|> doInstr <|> dontInstr))
  where
    findNext p = snd <$> manyTill_ anyChar p
    mulInstr = try $ do
      _ <- string "mul("
      n1 <- read <$> some digit
      _ <- string ","
      n2 <- read <$> some digit
      _ <- string ")"
      return (Mul n1 n2)
    doInstr = string "do()" >> pure Do
    dontInstr = string "don't()" >> pure Dont

part1 :: ByteString -> Int
part1 input = sum [a * b | Mul a b <- findInstructions input]

runProgram :: [Instruction] -> Int
runProgram = go True
  where
    go _ [] = 0
    go _ (Do:xs) = go True xs
    go _ (Dont:xs) = go False xs
    go p (Mul n1 n2:xs) = (if p then n1 * n2 else 0) + go True xs

part2 :: ByteString -> Int
part2 = runProgram . findInstructions
