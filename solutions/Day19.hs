{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module Day19 (part1, part2) where

import Control.Applicative.Combinators (sepBy, some)
import Control.Monad.Extra (anyM, filterM)
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.ST (ST, runST)
import Data.Attoparsec.ByteString.Char8 (endOfLine, letter_ascii)
import Data.ByteString (ByteString)
import Data.HashTable.ST.Basic (HashTable)
import Data.HashTable.ST.Basic qualified as HT
import Data.Hashable (Hashable)
import Data.List.Extra (stripPrefix)
import Util (parseOrError)

type Towel = String

type Design = String

newtype MemoM s k v a = MemoM (ReaderT (HashTable s k v) (ST s) a)
  deriving newtype (Functor, Applicative, Monad)

type Memo s k v = MemoM s k v v

memo :: (Hashable k) => (k -> MemoM s k v v) -> k -> MemoM s k v v
memo f k = MemoM $ ReaderT $ \tbl -> do
  val <- HT.lookup tbl k
  case val of
    Just v -> return v
    Nothing -> do
      let (MemoM (ReaderT f')) = f k
      v <- f' tbl
      HT.insert tbl k v
      return v

memoToST :: (forall s. MemoM s k v a) -> (forall s. HashTable s k v -> ST s a)
memoToST (MemoM x) = runReaderT x

runMemoM :: (forall s. MemoM s k v a) -> a
runMemoM m = runST $ do
  tbl <- HT.new
  memoToST m tbl

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
part1 input = runMemoM $ do
  let (available, desired) = readInput input
  length <$> filterM (canCreate available) desired

part2 :: ByteString -> Int
part2 input = runMemoM $ do
  let (available, desired) = readInput input
  sum <$> mapM (arrangements available) desired
