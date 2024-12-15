{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Exception (Exception, SomeException, catch, evaluate, throwIO, try)
import Control.Monad (forM, when, filterM, replicateM_, forM_, void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.Time.Calendar
import Data.Time.Clock
import Debug.Trace (traceMarkerIO)
import Network.HTTP.Simple
import Network.HTTP.Types
import Options.Applicative
import Solutions (IsSlow (..), Solution (..), displayAnswer, isSolvedAnswer, problemName, solutions)
import Day14 (part2Vis)
import System.Clock
import System.Directory
import System.Environment (getEnv)
import System.IO
import Text.Printf (printf)
import Control.Concurrent (newMVar, withMVar, newQSem, signalQSem, waitQSem, QSem)
import System.Console.ANSI (cursorUp, saveCursor, restoreCursor, cursorUpLine, setCursorColumn, clearScreen)
import Control.Concurrent.Async (async, wait)
import Data.Either (fromRight)
import qualified Day15
import Data.Functor ((<&>))

default (String)

titleLength :: Int
titleLength =
  maximum (map (length . problemName) solutions)

printTableAnchor :: AnchorType -> IO ()
printTableAnchor anchorType =
  printf
    "%s─%s─%s──────────────%s───────────\n"
    startMarker
    (replicate titleLength '─')
    middleMarker
    middleMarker
  where
    startMarker = case anchorType of
      Top -> "╭"
      Middle -> "├"
      Bottom -> "╰"

    middleMarker = case anchorType of
      Top -> "┬"
      Middle -> "┼"
      Bottom -> "┴"

printLineName :: String -> IO ()
printLineName name = do
  printf "│ %*s │ " titleLength name
  hFlush stdout

printLineAnswer :: TimeSpec -> String -> IO ()
printLineAnswer time answer =
  printf
    "%12s │ %s \n"
    formattedTime
    -- If the answer spans multiple lines, align it all in the right column of the table
    ( concatMap
        ( \case
            '\n' -> printf "\n│ %*s │        │ " titleLength ""
            c -> [c]
        )
        answer
    )
  where
    timeNanos :: Double
    timeNanos = fromIntegral $ toNanoSecs time

    formattedTime :: String
    formattedTime
      | timeNanos == 0 = ""
      | otherwise = printf "%.3f ms" (timeNanos / 1e6)

data AnchorType
  = Top
  | Middle
  | Bottom

data AOCError = InputNotOutYet
  deriving anyclass (Exception)

instance Show AOCError where
  show InputNotOutYet = "Input not out yet"

getAOCYear :: IO Year
getAOCYear =
  (read <$> getEnv "AOC_YEAR")
    `catch` ( \(_ :: IOError) -> do
                currentTime <- getCurrentTime
                let (year, month, _) = toGregorian $ utctDay currentTime
                return $
                  if month >= 12
                    then year
                    else year - 1
            )

solutionInputAvailable :: Int -> IO Bool
solutionInputAvailable day = do
  (currentYear, currentMonth, currentDay) <- toGregorian . utctDay <$> getCurrentTime
  aocYear <- getAOCYear

  return $
    if
      | currentYear < aocYear -> False
      | currentYear > aocYear -> True
      | currentMonth < 12 -> False
      | currentDay < day -> False
      | otherwise -> True

-- | Get input for a given day. The year is set by the AOC_YEAR environment
-- variable, or the current year if that is not set
downloadInput :: Int -> IO ByteString
downloadInput inputDay = do
  session_cookie <- getEnv "AOC_SESSION_COOKIE"
  aocYear <- getAOCYear

  let request =
        parseRequest_ (printf "https://adventofcode.com/%d/day/%d/input" aocYear inputDay)
          & addRequestHeader "Cookie" ("session=" <> BS.pack session_cookie)

  response <- httpBS request

  let status = getResponseStatus response
  when (statusCode status /= 200) $
    ioError $
      userError $
        "  Unexpected HTTP Response: "
          <> show (statusCode status)
          <> " "
          <> show (BS.unpack $ statusMessage status)

  return (getResponseBody response)

-- | Get input for a given day. If it is not downloaded, download it using @downloadInput@
getInput :: Int -> IO (Either SomeException ByteString)
getInput day = do
  available <- solutionInputAvailable day
  try $ do
    when (not available) (throwIO InputNotOutYet)

    BS.readFile inputPath `catch` \(_ :: IOError) -> do
      downloadedInput <- downloadInput day
      createDirectoryIfMissing False "data/"
      BS.writeFile inputPath downloadedInput
      -- Read the file to make sure that we get any possible issues with the file now, and not the next time it is read.
      BS.readFile inputPath
  where
    inputPath = "data/day" <> show day

withTiming :: String -> IO a -> IO (TimeSpec, a)
withTiming label a = do
  startTime <- getTime Monotonic
  traceMarkerIO ("Begin " ++ label)
  out <- a
  traceMarkerIO ("End " ++ label)
  timeElapsed <- diffTimeSpec startTime <$> getTime Monotonic
  return (timeElapsed, out)

runSolution :: Solution -> IO (TimeSpec, String)
runSolution solution@MkSolution {..} =
  getInput day >>= \case
    Left exc ->
      return (0, show exc)
    Right input -> do
      (timeElapsed, answer) <-
        withTiming (problemName solution) . try . evaluate $
          solve input
      case answer of
        Left (e :: SomeException) -> return (0, show e)
        Right v
          | isSolvedAnswer v -> return (timeElapsed, displayAnswer v)
          | otherwise -> return (0, "")

data Options
  = Day14
  | Day15 (Maybe FilePath)
  | MkOptions
    { solutionFilter :: Solution -> Bool
    , showFuture :: Bool
    , sequential :: Bool
    }

options :: Parser Options
options = do
  skipSlow <-
    asum
      [ flag'
          (\s -> s.isSlow /= Slow)
          (long "noslow" <> help "Skip slow solutions"),
        flag'
          (\s -> s.isSlow == Slow)
          (long "slow" <> help "Only run slow solutions"),
        pure (const True)
      ]
  day <-
    asum
      [ option
          ( do {day <- auto; return (\s -> s.day == day)} )
          (long "day" <> short 'd' <> help "Only run this day" <> metavar "DAY"),
        pure (const True)
      ]
  showFuture <- switch (long "show-future" <> help "Show days that are not available yet")
  sequential <- switch (long "sequential" <> help "Run solutions one-by-one")

  day14 <- switch (long "day14" <> help "Run the day 14 visualisation")
  day15 <-
    optional (
      (Just <$> strOption (long "day15-with" <> help "Run the day 15 visualisation"))
      <|> flag' Nothing (long "day15" <> help "Run the day 15 visualisation"))
  pure (if
           | day14 -> Day14
           | Just fp <- day15 -> Day15 fp
           | otherwise ->
             let solutionFilter s = skipSlow s && day s in MkOptions {..})

parseArgs :: IO Options
parseArgs =
  execParser $
    info
      (options <**> helper)
      (progDesc "Michalis' solutions to Advent of Code 2024")

main :: IO ()
main = parseArgs >>= \case
  Day14 -> do
    input <- getInput 14
    let images = part2Vis (fromRight undefined input)
    forM_ (zip [0::Int ..] images) $ \(i, image) -> do
      clearScreen
      printf "Iteration %d\n" i
      putStrLn image
      putStrLn ""
      void $ getLine
  Day15 fp -> do
    input <- case fp of
      Nothing -> fromRight (error "Missing input") <$> getInput 15
      Just path -> BS.readFile path
    Day15.interactive input
  MkOptions {solutionFilter, showFuture, sequential} -> do
    toRun <- solutions
      & filter solutionFilter
      & filterM (\MkSolution {day} -> (showFuture ||) <$> solutionInputAvailable day)

    -- Make space to fit the lines that will be printed
    replicateM_ (length toRun + 5) $ putStrLn ""
    cursorUp (length toRun + 5)

    ioLock <- newMVar ()
    let withIOLock = withMVar ioLock . const
    runLock <- newMVar ()
    let withRunLock
          | sequential = withMVar runLock . const
          | otherwise = id

    tasks <- withMVar ioLock $ \() -> do
      printTableAnchor Top
      tasks <- forM (zip [0..] toRun) $ \(idx, solution) -> do
        printLineName (problemName solution)
        putStrLn ""
        return $ do
          (timeElapsed, answer) <- withRunLock $
            runSolution solution
          withIOLock $ do
            saveCursor
            cursorUpLine (length toRun - idx + 1)
            setCursorColumn (titleLength + 5)
            printLineAnswer timeElapsed answer
            restoreCursor
          return timeElapsed
      printTableAnchor Middle
      return tasks

    times <- if sequential
      then sequence tasks
      else mapM wait =<< mapM async tasks

    printLineName "Total time"
    printLineAnswer (sum times) ""
    printTableAnchor Bottom
