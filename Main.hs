{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumDecimals #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module Main where

import Control.Exception (Exception, SomeException, catch, evaluate, throwIO, try)
import Control.Monad (forM, when)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Function ((&))
import Data.List (isInfixOf)
import Data.Time.Calendar
import Data.Time.Clock
import Debug.Trace (traceMarkerIO)
import Network.HTTP.Simple
import Network.HTTP.Types
import Solutions (Solution (..), displayAnswer, isSolvedAnswer, problemName, solutions)
import System.Clock
import System.Directory
import System.Environment (getArgs, getEnv)
import System.Exit (exitFailure)
import System.IO
import Text.Printf (printf)

default (String)

titleLength :: Int
titleLength =
  maximum (map (length . problemName) solutions)

data AnchorType
  = Top
  | Middle
  | Bottom

printTableAnchor :: AnchorType -> IO ()
printTableAnchor anchorType =
  printf
    "%s─%s─%s────────%s───────────\n"
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

printLine :: String -> TimeSpec -> String -> IO ()
printLine name time answer =
  printf
    "│ %*s │ %6s │ %s \n"
    titleLength
    name
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
    timeNanos :: Integer
    timeNanos = toNanoSecs time

    formattedTime :: String
    formattedTime
      | timeNanos == 0 = ""
      | timeNanos < 1e3 = printf "%d ns" timeNanos
      | timeNanos < 1e6 = printf "%d μs" (timeNanos `div` 1e3)
      | timeNanos < 1e9 = printf "%d ms" (timeNanos `div` 1e6)
      | otherwise = printf "%d  s" (timeNanos `div` 1e9)

data AOCError = InputNotOutYet
  deriving (Show)
  deriving anyclass (Exception)

currentAOCYear :: IO Year
currentAOCYear = do
  currentTime <- getCurrentTime
  let (year, month, _) = toGregorian $ utctDay currentTime
  return $
    if month >= 12
      then year
      else year - 1

-- | Get input for a given day. The year is set by the AOC_YEAR environment
-- variable, or the current year if that is not set
downloadInput :: Int -> IO ByteString
downloadInput inputDay = do
  (currentYear, currentMonth, currentDay) <- toGregorian . utctDay <$> getCurrentTime
  aocYear <- (read <$> getEnv "AOC_YEAR") `catch` (\(_ :: IOError) -> currentAOCYear)

  let inputIsAvailable
        | currentYear < aocYear = False
        | currentYear > aocYear = False
        | currentMonth < 12 = False
        | currentDay < inputDay = False
        | otherwise = True

  when (not inputIsAvailable) (throwIO InputNotOutYet)

  hPutStrLn stderr ("Downloading input for day " <> show inputDay)

  session_cookie <- getEnv "AOC_SESSION_COOKIE"

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
getInput :: Int -> IO ByteString
getInput day =
  BS.readFile inputPath `catch` \(_ :: IOError) -> do
    downloadedInput <- downloadInput day
    createDirectoryIfMissing False "data/"
    BS.writeFile inputPath downloadedInput
    -- Read the file to make sure that we get any possible issues with the file now, and not the next time it is read.
    BS.readFile inputPath
  where
    inputPath = "data/day" <> show day

main :: IO ()
main = do
  filterFun <-
    getArgs >>= \case
      [] -> return (const True)
      [filterString] -> return $ \solution -> filterString `isInfixOf` (problemName solution)
      _ -> putStrLn "Too many arguments" >> exitFailure

  solutionsWithInputs <- forM (filter filterFun solutions) $ \solution -> do
    ( do
        input <- getInput solution.day
        return (solution, Right input)
      )
      `catch` (\InputNotOutYet -> return (solution, Left "    (Input not yet available)"))
      `catch` (\(e :: SomeException) -> return (solution, Left (show e)))

  printTableAnchor Top
  times <- forM solutionsWithInputs $ \case
    (solution, Left msg) -> do
      printLine (problemName solution) 0 msg
      return 0
    (solution@MkSolution {solve}, Right input) -> do
      let name = problemName solution
      startTime <- getTime Monotonic
      traceMarkerIO ("Begin " ++ name)
      answer <-
        try (evaluate (solve input)) >>= \case
          Left (e :: SomeException) -> pure (Just (show e))
          Right v
            | isSolvedAnswer v -> pure (Just (displayAnswer v))
            | otherwise -> pure Nothing
      traceMarkerIO ("End " ++ name)
      timeElapsed <- diffTimeSpec startTime <$> getTime Monotonic
      case answer of
        Nothing -> do
          printLine name 0 ""
          return 0
        Just s -> do
          printLine name timeElapsed s
          return timeElapsed

  printTableAnchor Middle
  printLine "Total time" (sum times) ""
  printTableAnchor Bottom
