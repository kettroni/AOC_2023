{-# LANGUAGE OverloadedStrings #-}
module D5 where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe (fromJust, mapMaybe, isJust)
import Data.List (uncons, groupBy, sort, find)
import Data.Char (isDigit)

day5part1 :: B.ByteString -> String
day5part1 bs = show $ part1 bs

part1 bs = minimum $ applyMaps maps <$> seeds
  where
    (seedLine, restLines) = fromJust . Data.List.uncons $ getRelevantLines bs
    seeds = parseSeedLine1 (head seedLine)
    mapLines = tail . tail <$> restLines
    maps = parseMapLine <$> mapLines

getRelevantLines :: B.ByteString -> [[B.ByteString]]
getRelevantLines = Data.List.groupBy (\_ l2 -> l2 /= "") . B.lines

parseSeedLine1 :: B.ByteString -> [Int]
parseSeedLine1 = numbersFromLine . B.dropWhile (not . isDigit)

numbersFromLine :: B.ByteString -> [Int]
numbersFromLine = (intFromBs <$>) . B.words

intFromBs :: B.ByteString -> Int
intFromBs = fst . fromJust . B.readInt

parseMapLine :: [B.ByteString] -> [Int -> Maybe Int]
parseMapLine bss = mappingFunc . numbersFromLine <$> bss

mappingFunc :: [Int] -> (Int -> Maybe Int)
mappingFunc [dest, src, len] seed = if isBetween seed src (src + (len - 1))
                                                    then Just (seed - src + dest)
                                                    else Nothing

tupleFromList :: [a] -> (a, a)
tupleFromList xs = (head xs, (head . tail) xs)

isBetween :: Int -> Int -> Int -> Bool
isBetween n lower upper = n >= lower && n <= upper

applyMap :: [a -> Maybe a] -> a -> a
applyMap fs a = case mapMaybe (\f -> f a) fs of
  [] -> a
  as -> head as

applyMaps :: [[a -> Maybe a]] -> a -> a
applyMaps ms curr = foldl (flip applyMap) curr ms

day5part2 :: B.ByteString -> String
day5part2 bs = show $ part2 bs

part2 bs = (applyMap . head) maps 57
  where
    (seedLine, restLines) = fromJust . Data.List.uncons $ getRelevantLines bs
    seeds = parseSeedLine2 (head seedLine)
    mapLines = tail . tail <$> reverse restLines
    maps = parseMapLine2 <$> mapLines
    locs = [0..]
    -- ans = fromJust $ find (\l -> locationFoundInSeeds l maps seeds) locs

locationFoundInSeeds :: Int -> [[Int -> Maybe Int]] -> [Int] -> Bool
locationFoundInSeeds loc fs seeds = isJust $ Data.List.find (== a) seeds
  where
    a = applyMaps fs loc

parseSeedLine2 :: B.ByteString -> [Int]
parseSeedLine2 = Data.List.sort . concatMap (\t -> [fst t .. uncurry (+) t -1 ]) . parseTupleWhile . parseSeedLine1

parseTupleWhile :: [a] -> [(a, a)]
parseTupleWhile (a1:a2:as) = tupleFromList [a1,a2] : parseTupleWhile as
parseTupleWhile _ = []

parseMapLine2 :: [B.ByteString] -> [Int -> Maybe Int]
parseMapLine2 bss = mappingFunc2 . numbersFromLine <$> bss

mappingFunc2 :: [Int] -> (Int -> Maybe Int)
mappingFunc2 [dest, src, len] = \seed -> if isBetween seed dest (dest + (len - 1))
                                                    then Just (seed - dest + src)
                                                    else Nothing
