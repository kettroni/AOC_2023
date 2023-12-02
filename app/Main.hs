{-#  LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import System.Environment (getArgs)
import Data.List (find)

main :: IO ()
main = do
  args <- getArgs
  runSolution args

type Input = B.ByteString
type Output = String
type Solution = Input -> Output

runSolution :: [String] -> IO ()
runSolution args =
  let
    argInBS = B.pack $ head args
    (solution, path) = solutionByArg argInBS
  in
    do
      bs <- B.readFile path
      print $ solution bs

solutionByArg :: B.ByteString -> (Solution, String)
solutionByArg "1.1" = (day1part1, "data/day1.txt")
solutionByArg "1.2" = (day1part2, "data/day1.txt")
solutionByArg _ = (day1part1, "data/day1.txt")

day1part1 :: Solution
day1part1 = show . day1 intsFromString1

type Day1Strategy = B.ByteString -> Maybe Int

day1 :: Day1Strategy -> B.ByteString -> Int
day1 f bs = sum $ mapMaybe f (B.lines bs)

intsFromString1 :: Day1Strategy
intsFromString1 bs =
  let
    findDigit :: B.ByteString -> Maybe Char
    findDigit s = B.find isDigit s

    firstAndLastDigits :: [Char]
    firstAndLastDigits = mapMaybe findDigit [bs, B.reverse bs]
  in
    do
      (int, _) <- B.readInt $ B.pack firstAndLastDigits
      pure int

day1part2 :: Solution
day1part2 = show . day1 intsFromString2

intsFromString2 :: Day1Strategy
intsFromString2 bs =
  let
    firstAndLastDigits :: [Char]
    firstAndLastDigits = mapMaybe (\x -> x) [firstDigit bs, lastDigit (B.reverse bs)]
  in
    do
      (int, _) <- B.readInt $ B.pack firstAndLastDigits
      pure int

headIsDigit :: B.ByteString -> Bool
headIsDigit = isDigit . B.head

firstDigit :: B.ByteString -> Maybe Char
firstDigit "" = Nothing
firstDigit s = if headIsDigit s
  then Just (B.head s)
  else case getNumberPrefix s of
    Just prefix -> prefixesToDigit prefix
    Nothing -> firstDigit (B.tail s)

lastDigit :: B.ByteString -> Maybe Char
lastDigit "" = Nothing
lastDigit s = if headIsDigit s
  then Just (B.head s)
  else case getNumberSuffix s of
    Just suffix -> prefixesToDigit $ B.reverse suffix
    Nothing -> lastDigit (B.tail s)

getPrefix :: B.ByteString -> [B.ByteString] -> Maybe B.ByteString
getPrefix s = find (\x -> B.isPrefixOf x s)

getNumberPrefix :: B.ByteString -> Maybe B.ByteString
getNumberPrefix s = getPrefix s prefixes

getNumberSuffix :: B.ByteString -> Maybe B.ByteString
getNumberSuffix s = getPrefix s suffixes

prefixes :: [B.ByteString]
prefixes = [ "one"
           , "two"
           , "three"
           , "four"
           , "five"
           , "six"
           , "seven"
           , "eight"
           , "nine"
           ]

suffixes :: [B.ByteString]
suffixes = B.reverse <$> prefixes

prefixesToDigit :: B.ByteString -> Maybe Char
prefixesToDigit "one" = Just '1'
prefixesToDigit "two" = Just '2'
prefixesToDigit "three" = Just '3'
prefixesToDigit "four" = Just '4'
prefixesToDigit "five" = Just '5'
prefixesToDigit "six" = Just '6'
prefixesToDigit "seven" = Just '7'
prefixesToDigit "eight" = Just '8'
prefixesToDigit "nine" = Just '9'
prefixesToDigit _ = Nothing
