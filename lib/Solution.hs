module Solution where

import qualified Data.ByteString.Lazy.Char8 as B
import Utils (Solution)
import D1 (day1part1, day1part2)
import D2 (day2part1, day2part2)
import D3 (day3part1, day3part2)
import D4 (day4part1, day4part2)
import D5 (day5part1)

runSolution :: [String] -> IO ()
runSolution args =
  let
    argInBS = B.pack $ head args
    (solution, path) = solutionByArg argInBS
  in
    do
      bs <- B.readFile path
      print $ solution bs

getSolution :: [String] -> IO (String)
getSolution args =
  let
    argInBS = B.pack $ head args
    (solution, path) = solutionByArg argInBS
  in
    do
      bs <- B.readFile path
      pure $ solution bs

solutionByArg :: B.ByteString -> (Solution, String)
-- Day 1
solutionByArg "1.1" = (day1part1, "data/day1.txt")
solutionByArg "1.1e" = (day1part1, "data/example1.txt")
solutionByArg "1.2" = (day1part2, "data/day1.txt")
solutionByArg "1.2e" = (day1part2, "data/example1_2.txt")

-- Day 2
solutionByArg "2.1" = (day2part1, "data/day2.txt")
solutionByArg "2.1e" = (day2part1, "data/example2.txt")
solutionByArg "2.2" = (day2part2, "data/day2.txt")
solutionByArg "2.2e" = (day2part2, "data/example2.txt")

-- Day 3
solutionByArg "3.1" = (day3part1, "data/day3.txt")
solutionByArg "3.1e" = (day3part1, "data/example3.txt")
solutionByArg "3.2" = (day3part2, "data/day3.txt")
solutionByArg "3.2e" = (day3part2, "data/example3.txt")

-- Day 4
solutionByArg "4.1" = (day4part1, "data/day4.txt")
solutionByArg "4.1e" = (day4part1, "data/example4.txt")
solutionByArg "4.2" = (day4part2, "data/day4.txt")
solutionByArg "4.2e" = (day4part2, "data/example4.txt")

-- Day 5
solutionByArg "5.1" = (day5part1, "data/day5.txt")
solutionByArg "5.1e" = (day5part1, "data/example5.txt")
solutionByArg _ = (day1part1, "data/day1.txt")
