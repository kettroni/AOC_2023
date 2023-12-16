module D4 where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import Data.List (find)
import Utils (Solution)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

day4part1 :: Solution
day4part1 bs = show $ sum c
  where
    cardHalves = B.dropWhile (not . isDigit) . B.dropWhile (/= ':') <$> B.lines bs
    numbersPairStrings = mapTuple B.words . B.break (== '|') <$> cardHalves
    a = mapTuple (mapMaybe B.readInt) <$> numbersPairStrings
    pairLists = mapTuple (fst <$>) <$> a
    c = (\n -> powerOfTwo $ length n - 1) <$> do
      (ws, ms) <- pairLists
      pure $ do
        w <- ws
        guard $ elem w ms
        pure w

powerOfTwo :: Int -> Int
powerOfTwo n
  | n < 0 = 0
  | otherwise = 2^n

day4part2 :: Solution
day4part2 bs = show $ totalCards
  where
    halves = mapTuple (B.dropWhile (not . isDigit)) . B.break (== ':') <$> B.lines bs
    numbersPairStrings = mapTuple B.words . B.break (== '|') . snd <$> halves
    a = mapTuple (mapMaybe B.readInt) <$> numbersPairStrings
    pairLists = mapTuple (fst <$>) <$> a
    originalCards = initialCardWins 1 $ length <$> do
      (winNs, myNs) <- pairLists
      pure $ do
        w <- winNs
        guard $ elem w myNs
        pure w
    copyCards = winCards originalCards <$> originalCards
    totalCards = sum $ length <$> copyCards

initialCardWins :: Int -> [Int] -> [CardWins]
initialCardWins _ [] = []
initialCardWins i (a:as) = CardWins { cardId = i, wins = a } : initialCardWins (i+1) as

data CardWins = CardWins { cardId :: Int, wins :: Int } deriving Show

winCards :: [CardWins] -> CardWins -> [CardWins]
winCards cache src = src : concat n
  where
    findCacheWins i = case find ((==i) . cardId) cache of
      Just cw -> wins cw
      Nothing -> 0
    a = (\i -> CardWins { cardId = i, wins = findCacheWins i }) <$> (+ cardId src) <$> [1..wins src]
    n = winCards cache <$> a
