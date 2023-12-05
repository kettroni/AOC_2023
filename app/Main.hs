module Main where

import Solution
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  runSolution args
