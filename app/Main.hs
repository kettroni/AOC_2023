{-#  LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Data.Maybe (catMaybes)
import Control.Monad (guard)

main :: IO ()
main = day1part1

getLines :: FilePath -> IO ([B.ByteString])
getLines = fmap B.lines . B.readFile

day1part1 :: IO ()
day1part1 = day1 intsFromString1

day1 :: (B.ByteString -> Maybe Int) -> IO ()
day1 f = do
  ls <- getLines "data/day1_1.txt"
  print $ sum . catMaybes $ map f ls

intsFromString1 :: B.ByteString -> Maybe Int
intsFromString1 bs =
  let
    firstDigit = (B.singleton <$>) . B.find isDigit
  in
    do
      intStr <- firstDigit bs <> firstDigit (B.reverse bs)
      (int, _) <- B.readInt intStr
      return $ int
