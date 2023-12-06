{-# LANGUAGE OverloadedStrings #-}
module D5 where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust, mapMaybe)
import Data.List (uncons, groupBy, scanl1)
import Data.Char (isDigit)

day5part1 :: B.ByteString -> String
day5part1 bs = show $ part1 bs

part1 bs = minimum $ applyMaps maps <$> seeds
  where
    (seedLine, restLines) = (fromJust . uncons) $ getRelevantLines bs
    seeds = parseSeedLine (head seedLine)
    mapLines = (fromJust . uncons . tail) <$> restLines
    maps = parseMapLine <$> mapLines

getRelevantLines :: B.ByteString -> [[B.ByteString]]
getRelevantLines = groupBy (\_ l2 -> l2 /= "") . B.lines

parseSeedLine :: B.ByteString -> [Int]
parseSeedLine = (numbersFromLine . B.dropWhile (not . isDigit))

numbersFromLine :: B.ByteString -> [Int]
numbersFromLine = (intFromBs <$>) . B.words

intFromBs :: B.ByteString -> Int
intFromBs = fst . fromJust . B.readInt

parseMapLine :: (B.ByteString, [B.ByteString]) -> [Int -> Maybe Int]
parseMapLine (bs, bss) = parseMapNumbersPart . numbersFromLine <$> bss
  where
    parsedMapTextPart = (tupleFromList . filter (/= "to") . B.splitWith (=='-') . head . B.words) bs
    parseMapNumbersPart = seedToSoilFunc

seedToSoilFunc :: [Int] -> (Int -> Maybe Int)
seedToSoilFunc [dest, src, len] = \seed -> if isBetween seed src (src + (len - 1))
                                                    then Just (seed - src + dest)
                                                    else Nothing

tupleFromList :: [a] -> (a, a)
tupleFromList xs = (head xs, (head . tail) xs)

isBetween :: Int -> Int -> Int -> Bool
isBetween n lower upper = n >= lower && n <= upper

applyMap :: [(a -> Maybe a)] -> a -> a
applyMap fs a = case mapMaybe (\f -> f a) fs of
  [] -> a
  as -> head as

applyMaps :: [[a -> Maybe a]] -> a -> a
applyMaps [] curr = curr
applyMaps (m:ms) curr = applyMaps ms (applyMap m curr)
