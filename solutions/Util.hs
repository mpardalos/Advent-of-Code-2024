{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Util where

import Control.Concurrent (forkIO)
import Control.Monad (void, (>=>))
import Control.Monad.ST (ST, runST)
import Data.Array (Array)
import Data.Array.IArray (IArray, Ix, assocs, listArray)
import Data.Array.MArray (MArray, freeze, thaw)
import Data.Array.ST (STArray, STUArray)
import Data.Array.Unboxed (UArray)
import Data.Attoparsec.ByteString.Char8 (Parser, endOfLine, parseOnly, sepBy)
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Graph.Inductive (Graph, Node)
import Data.GraphViz (GraphvizCanvas (Xlib), GraphvizCommand (Dot), GraphvizParams, Labellable, graphToDot, preview, quickParams, runGraphvizCanvas, runGraphvizCanvas', setDirectedness)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List (groupBy, intercalate, unfoldr)
import Data.Set qualified as Set
import Debug.Trace (trace)
import GHC.IO.Handle (hPutStr)
import System.Process
  ( CreateProcess (std_in),
    ProcessHandle,
    StdStream (CreatePipe),
    createProcess,
    proc,
  )

parseOrError :: Parser a -> ByteString -> a
parseOrError parser input = case parseOnly parser input of
  Left e -> error ("Parse error: " <> show e)
  Right v -> v

hashNub :: (Hashable a) => [a] -> [a]
hashNub = HashSet.toList . HashSet.fromList

ordNub :: (Ord a) => [a] -> [a]
ordNub = Set.toList . Set.fromList

equating :: (Eq a) => (b -> a) -> b -> b -> Bool
equating f x y = f x == f y

readIntsSepBy :: Char -> ByteString -> [Int]
readIntsSepBy c = unfoldr (BS.readInt . (BS.dropWhile (== c)))

readSpacedInts :: ByteString -> [Int]
readSpacedInts = unfoldr (BS.readInt . BS.dropSpace)

readVerticalLists :: ByteString -> ([Int], [Int])
readVerticalLists input =
  BS.lines input
    & map BS.words
    & map (\[l, r] -> (l, r))
    & map (bimap (read . BS.unpack) (read . BS.unpack))
    & unzip

pairwise :: [a] -> [(a, a)]
pairwise (a : b : rest) = (a, b) : pairwise (b : rest)
pairwise _ = []

pairwiseSeparate :: [a] -> [(a, a)]
pairwiseSeparate (a : b : rest) = (a, b) : pairwiseSeparate rest
pairwiseSeparate _ = []

choose :: [a] -> Int -> [[a]]
choose _ 0 = [[]]
choose [] _ = []
choose (x : xs) n = map (x :) (xs `choose` (n - 1)) ++ xs `choose` n

-- | Get all unordered pairs of elements from a list.
-- The cartesian product of the list with itself
uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x : xs) = [(x, y) | y <- xs] ++ uniquePairs xs

orderedPairs :: [a] -> [(a, a)]
orderedPairs = uniquePairs >=> \(a, b) -> [(a, b), (b, a)]

manhattanDistance :: (Num a) => (a, a) -> (a, a) -> a
manhattanDistance (row1, col1) (row2, col2) = abs (row2 - row1) + abs (col2 - col1)

traceShowIdLabelled :: (Show a) => String -> a -> a
traceShowIdLabelled l x = trace (l ++ ": " ++ show x) x

graphvizView :: String -> IO ProcessHandle
graphvizView dot = do
  (Just hGvIn, _, _, gvProcess) <- createProcess $ (proc "dot" ["-Tx11"]) {std_in = CreatePipe}
  hPutStr hGvIn dot
  return gvProcess

previewWithCmd :: (Graph gr, Ord el, Labellable l, Labellable el) => GraphvizCommand -> gr l el -> IO ()
previewWithCmd cmd = previewWith cmd quickParams

previewWithParams :: (Graph gr, Ord el, Ord cl) => GraphvizParams Node nl el cl l -> gr nl el -> IO ()
previewWithParams = previewWith Dot

previewWith :: (Graph gr, Ord el, Ord cl) => GraphvizCommand -> GraphvizParams Node nl el cl l -> gr nl el -> IO ()
previewWith cmd params g = void $ forkIO $ runGraphvizCanvas cmd (setDirectedness graphToDot params g) Xlib

unsafeParse :: Parser a -> ByteString -> a
unsafeParse parser bs = case parseOnly parser bs of
  Right r -> r
  Left e -> error ("Parse failure: " ++ show e)

expecting :: (a -> Bool) -> a -> a
expecting p x
  | p x = x
  | otherwise = error "expecting"

expectingNote :: String -> (a -> Bool) -> a -> a
expectingNote note p x
  | p x = x
  | otherwise = error ("expected " ++ note)

findAllIndices :: (Ix i, IArray a e) => (e -> Bool) -> a i e -> [i]
findAllIndices p = map fst . filter (p . snd) . assocs

type Coords = (Int, Int)

type Grid c = Array Coords c

type UGrid c = UArray Coords c

type STGrid s c = STArray s Coords c

type STUGrid s c = STUArray s Coords c

-- readDenseGrid :: ByteString -> Grid Char
readDenseGrid :: (IArray a Char) => ByteString -> a (Int, Int) Char
readDenseGrid input =
  let chars :: [Char] = concatMap BS.unpack (BS.lines input)
      columns = BS.length (head (BS.lines input))
      rows = length (BS.lines input)
   in listArray ((0, 0), (rows - 1, columns - 1)) chars

west :: Coords -> Int -> Coords
west (row, col) n = (row, col - n)

south :: Coords -> Int -> Coords
south (row, col) n = (row + n, col)

east :: Coords -> Int -> Coords
east (row, col) n = (row, col + n)

north :: Coords -> Int -> Coords
north (row, col) n = (row - n, col)

rows :: Grid a -> [[a]]
rows = map (map snd) . groupBy (equating (fst . fst)) . assocs

showCharGrid :: Grid Char -> String
showCharGrid = intercalate "\n" . rows

showGridOneChar :: (Show a) => Grid a -> String
showGridOneChar = showCharGrid . fmap (head . show)

linesOf :: Parser a -> Parser [a]
linesOf p = p `sepBy` endOfLine

readChar :: (Read a) => Char -> a
readChar = read . pure

infixl 4 <<$>>

-- | Like '<$>', but through two levels of functors
(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) f x = fmap (fmap f) x

withMutableArray ::
  (Ix i, (forall s. MArray (a s) e (ST s)), IArray b e) =>
  (forall s. (a s) i e -> ST s ()) ->
  b i e ->
  b i e
withMutableArray f arr = runST $ do
  mutArr <- thaw arr
  f mutArr
  freeze mutArr
{-# INLINE withMutableArray #-}
