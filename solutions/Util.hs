{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Util where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Data.Attoparsec.ByteString.Char8 (Parser, parseOnly)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Graph.Inductive (Graph, Node)
import Data.GraphViz (GraphvizCanvas (Xlib), GraphvizCommand (Dot), GraphvizParams, Labellable, graphToDot, preview, quickParams, runGraphvizCanvas, runGraphvizCanvas', setDirectedness)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List (unfoldr)
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

hashNub :: Hashable a => [a] -> [a]
hashNub = HashSet.toList . HashSet.fromList

ordNub :: Ord a => [a] -> [a]
ordNub = Set.toList . Set.fromList

readIntsSepBy :: Char -> ByteString -> [Int]
readIntsSepBy c = unfoldr (BS.readInt . (BS.dropWhile (== c)))

readSpacedInts :: ByteString -> [Int]
readSpacedInts = unfoldr (BS.readInt . BS.dropSpace)

pairwise :: [a] -> [(a, a)]
pairwise (a : b : rest) = (a, b) : pairwise (b : rest)
pairwise _ = []

choose :: [a] -> Int -> [[a]]
choose _ 0 = [[]]
choose [] _ = []
choose (x : xs) n = map (x :) (xs `choose` (n - 1)) ++ xs `choose` n

-- | Get all unordered pairs of elements from a list.
-- The cartesian product of the list with itself
uniquePairs :: [a] -> [(a, a)]
uniquePairs [] = []
uniquePairs (x : xs) = [(x, y) | y <- xs] ++ uniquePairs xs

manhattanDistance :: Num a => (a, a) -> (a, a) -> a
manhattanDistance (row1, col1) (row2, col2) = abs (row2 - row1) + abs (col2 - col1)

traceShowIdLabelled :: Show a => String -> a -> a
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

infixl 4 <<$>>

-- | Like '<$>', but through two levels of functors
(<<$>>) :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<<$>>) f x = fmap (fmap f) x
