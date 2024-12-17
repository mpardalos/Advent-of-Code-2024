{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Day17 (part1, part2) where

import Control.Applicative.Combinators (sepBy)
import Control.Monad (guard, when)
import Control.Monad.State (StateT, execStateT, gets)
import Data.Attoparsec.ByteString.Char8 (decimal, endOfLine)
import Data.Bits (xor, (.&.), (.>>.))
import Data.ByteString (ByteString)
import Data.Foldable (toList)
import Data.List (intercalate, unfoldr)
import Data.Tuple.Extra (dupe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Optics
import Optics.State.Operators ((%=), (.=))
import Text.Printf (printf)
import Util (pairwiseSeparate, parseOrError)

data Computer v = MkComputer
  { a :: !v,
    b :: !v,
    c :: !v,
    pc :: !Int,
    memory :: !(Vector Int),
    output :: !(Vector v)
  }

makeFieldLabelsNoPrefix ''Computer

class Value v where
  fromInt :: Int -> v

instance Value Int where
  fromInt = id

instance Show v => Show (Computer v) where
  show MkComputer {..} =
    unlines
      [ printf "Register A: %s" (show a),
        printf "Register B: %s" (show b),
        printf "Register C: %s" (show c),
        "",
        program,
        "",
        printf "Output: %s" (show output)
      ]
    where
      program =
        memory
          & V.toList
          & zip [0 :: Int ..]
          & pairwiseSeparate
          & map showInstruction
          & unlines
        where
          showInstruction ((aInstr, instr), (_aOp, op)) =
            let prefix :: String = if aInstr == pc then ">" else " "
                operation = case instr of
                  0 -> "adv " ++ combo
                  1 -> "bxl " ++ literal
                  2 -> "bst " ++ combo
                  3 -> "jnz " ++ literal
                  4 -> "bxc " ++ ignore
                  5 -> "out " ++ combo
                  6 -> "bdv " ++ combo
                  7 -> "cdv " ++ combo
                  _ -> show instr ++ show op
                ignore = "(" ++ show op ++ ")"
                literal = "#" ++ show op
                combo = case op of
                  _ | op <= 3 -> "#" ++ show op
                  4 -> "A"
                  5 -> "B"
                  6 -> "C"
                  _ -> show op
             in printf "%s | %s" prefix operation

type ComputerM v a = StateT (Computer v) Maybe a

readInput :: ByteString -> Computer Int
readInput = parseOrError $ do
  a <- "Register A: " *> decimal <* endOfLine
  b <- "Register B: " *> decimal <* endOfLine
  c <- "Register C: " *> decimal <* endOfLine
  endOfLine
  memory <- V.fromList <$> ("Program: " *> (decimal `sepBy` ","))
  return MkComputer {pc = 0, output = [], ..}

readComboOperand :: Value v => Int -> ComputerM v v
readComboOperand n | n <= 3 = pure (fromInt n)
readComboOperand 4 = use #a
readComboOperand 5 = use #b
readComboOperand 6 = use #c
readComboOperand n = error ("Invalid combo operand: " ++ show n)

concreteStep :: Computer Int -> Maybe (Computer Int)
concreteStep = execStateT $ do
  guard =<< gets (\st -> st.pc <= length st.memory - 2)
  [instruction, operand] <- gets (\st -> V.slice st.pc 2 st.memory)
  #pc %= (+ 2)
  case instruction of
    0 {- adv -} -> do
      numerator <- use #a
      denominator <- readComboOperand operand
      #a .= numerator .>>. denominator
    1 {- bxl -} -> do
      #b %= xor operand
    2 {- bst -} -> do
      value <- readComboOperand operand
      #b .= (value .&. 0b111)
    3 {- jnz -} -> do
      a <- use #a
      when (a /= 0) $ do
        #pc .= operand
    4 {- bxc -} -> do
      c <- use #c
      #b %= xor c
    5 {- out -} -> do
      value <- readComboOperand operand
      #output %= (<> [value .&. 0b111])
    6 {- bdv -} -> do
      numerator <- use #a
      denominator <- readComboOperand operand
      #b .= numerator .>>. denominator
    7 {- cdv -} -> do
      numerator <- use #a
      denominator <- readComboOperand operand
      #c .= numerator .>>. denominator
    _ -> error ("Invalid instruction: " ++ show instruction)

runToCompletion :: Computer Int -> [Computer Int]
runToCompletion = unfoldr (fmap dupe . concreteStep)

formatOutput :: (Foldable t) => t Int -> String
formatOutput = intercalate "," . map show . toList

part1 :: ByteString -> String
part1 input =
  readInput input
    & runToCompletion
    & last
    & view #output
    & V.toList
    & formatOutput

part2 :: ByteString -> ()
part2 _ = ()
