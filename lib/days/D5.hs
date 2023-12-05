{-# LANGUAGE OverloadedStrings #-}
module D5 where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust)
import Data.List (uncons, groupBy)
import Data.Char (isDigit)

type Seed = Int
type Soil = Int
type Fertilizer = Int
type Water = Int
type Light = Int
type Temperature = Int
type Humidity = Int
type Location = Int

day5part1 :: B.ByteString -> String
day5part1 bs = show rest
  where
    relevantLines = getRelevantLines bs
    (seedLine, restLines) = (fromJust . uncons) relevantLines
    seeds = parseSeedLine seedLine
    -- rest = (\t -> (parseMapLine . fst t, snd t)) . B.splitWith (==':') <$> restLines
    rest = B.splitWith (==':') <$> restLines

getRelevantLines :: B.ByteString -> [B.ByteString]
getRelevantLines = (foldl (<>) "" <$>) . groupBy (\_ l2 -> l2 /= "") . B.lines

parseSeedLine :: B.ByteString -> [Seed]
parseSeedLine = (numbersFromLine . B.dropWhile (not . isDigit))

numbersFromLine :: B.ByteString -> [Int]
numbersFromLine = (intFromBs <$>) . B.words

intFromBs :: B.ByteString -> Int
intFromBs = fst . fromJust . B.readInt

parseMapLine :: B.ByteString -> (B.ByteString, B.ByteString)
parseMapLine = tupleFromList . filter (/= "to") . B.splitWith (=='-') . fst . fromJust . uncons . B.words

tupleFromList :: [a] -> (a, a)
tupleFromList xs = (head xs, (head . tail) xs)
