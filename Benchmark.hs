module Main where

import Control.Exception (try)
import Control.Monad (forM)
import Criterion.Main
  ( bench,
    bgroup,
    defaultConfig,
    defaultMainWith,
    nf,
    whnf,
  )
import Criterion.Types (Config (..))
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (catMaybes)
import Solutions (Solution (..), displayAnswer, inputFileName, isSolved, problemName, solutions)

main :: IO ()
main = do
  solutionsWithInputs :: [(Solution, ByteString)] <-
    fmap catMaybes $ forM solutions $ \solution -> do
      inputResult <- try @IOError (BS.readFile ("data/" <> (inputFileName solution)))
      case (inputResult, isSolved solution) of
        (Right input, True) -> return (Just (solution, input))
        _ -> return Nothing

  defaultMainWith
    defaultConfig {reportFile = Just "benchmark.html"}
    [ bgroup
        "solutions"
        [ bench (problemName solution) (whnf solve input)
          | (solution@MkSolution {solve}, input) <- solutionsWithInputs
        ],
      bench "All together" $
        nf
          (map (\(MkSolution {solve}, input) -> displayAnswer $ solve input))
          solutionsWithInputs
    ]
