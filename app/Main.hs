{-#  LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit, isSpace)
import Data.Maybe (mapMaybe, catMaybes)
import System.Environment (getArgs)
import Data.List (find, sort)
import Control.Monad (guard)

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
solutionByArg "2.1" = (day2part1, "data/day2.txt")
solutionByArg "2.2" = (day2part2, "data/day2.txt")
solutionByArg "3.1" = (day3part1, "data/day3.txt")
solutionByArg "3.2" = (day3part2, "data/day3.txt")
solutionByArg "4.1" = (day4part1, "data/day4.txt")
solutionByArg "4.1e" = (day4part1, "data/example4.txt")
solutionByArg "4.2" = (day4part2, "data/day4.txt")
solutionByArg "4.2e" = (day4part2, "data/example4.txt")
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

data Bag = Bag { red :: Int
               , green :: Int
               , blue :: Int
               } deriving Show

instance Semigroup Bag where
  b1 <> b2 = Bag { red   = maximum $ red   <$> [b1, b2]
                 , green = maximum $ green <$> [b1, b2]
                 , blue  = maximum $ blue  <$> [b1, b2]
                 }

instance Monoid Bag where
  mempty = Bag { red = 0
               , green = 0
               , blue = 0
               }

data Game = Game { identifier :: Int
                 , maximumBag :: Bag
                 } deriving Show

game :: Int -> [Bag] -> Game
game i reveals = Game { identifier = i
                      , maximumBag = mconcat reveals
                      }

withinCapacity :: Bag -> Bag -> Bool
withinCapacity capBag b =
  (red capBag) >= (red b)
  && (green capBag) >= (green b)
  && (blue capBag) >= (blue b)

day2part1 :: Solution
day2part1 bs = show $ sum . map (identifier) $ filter (withinCapacity capacityBag . maximumBag) $ parseGame <$> B.lines bs
  where
    capacityBag = Bag { red = 12
                      , green = 13
                      , blue = 14
                      }

parseGame :: B.ByteString -> Game
parseGame bs = game n reveals
  where
    (gameIdPart, bagsPart) = B.span (/= ':') bs
    n = parseId gameIdPart
    reveals = do
      reveal <- (map (B.dropWhile (not . isDigit))) <$> ((B.split ',') . B.dropWhile (not . isDigit)) <$> (B.split ';' bagsPart)
      pure $ Bag { red = parseRevealColor reveal " red"
                 , green = parseRevealColor reveal " green"
                 , blue = parseRevealColor reveal " blue"
                 }

parseId :: B.ByteString -> Int
parseId gamePart = maybe 0 id $ do
  (a, _) <- B.readInt $ B.dropWhile (not . isDigit) gamePart
  pure a

parseRevealColor :: [B.ByteString] -> B.ByteString -> Int
parseRevealColor reveal color = sum . map fst $ filter ((== color) . snd) $ ((maybe (0, color) id) . B.readInt) <$> reveal

day2part2 :: Solution
day2part2 bs = show . sum $ power . parseGame <$> B.lines bs

power :: Game -> Int
power (Game _ g) = (red g) * (green g) * (blue g)

day3part1 :: Solution
day3part1 bs = show . sum $ itemToInt <$> numbersNextToSymbols
  where
    ns = nsFromBs (makeLocation 0 0) bs
    symbols = filter ((== Symbol) . item) ns
    numbers = filter ((/= Symbol) . item) ns
    numbersNextToSymbols = item <$> (numbers `nextTo` (location <$> symbols))

nextTo :: [N] -> [Location] -> [N]
nextTo srcNs locs = filter (anyLocationNextTo locs . nLocations) srcNs

anyLocationNextTo :: [Location] -> [Location] -> Bool
anyLocationNextTo ls1 ls2 = any (\t -> locationNextToLocation (fst t) (snd t)) $ do
  l1 <- ls1
  l2 <- ls2
  pure (l1, l2)

nLocations :: N -> [Location]
nLocations (N { location = l1@Location { x = x1, y = y1 }, item = i1 }) = case i1 of
  Symbol -> [l1]
  Num n -> ((flip makeLocation) y1) . (+ x1) <$> [0 .. (length $ show $ n) - 1]

locationNextToLocation :: Location -> Location -> Bool
locationNextToLocation (Location { x = x1, y = y1 }) (Location { x = x2, y = y2 }) =
  (absDiff x1 x2) < 2 && (absDiff y1 y2) < 2
    where
      absDiff n = abs . (-) n

data Item = Num Int | Symbol
instance Show Item where
  show (Num a) = show a
  show Symbol = "Symbol"
instance Eq Item where
  Num n1 == Num n2 = n1 == n2
  Symbol == Symbol = True
  _ == _ = False

itemToInt :: Item -> Int
itemToInt (Num n1) = n1
itemToInt _ = 0

data Location = Location { x :: Int, y :: Int } deriving Show

makeLocation :: Int -> Int -> Location
makeLocation x1 y1 = Location { x = x1, y = y1 }

data N = N { location :: Location
           , item :: Item
           } deriving Show

nsFromBs :: Location -> B.ByteString -> [N]
nsFromBs loc bs = case B.uncons bs of
  Just (h, rest) -> help1 h rest loc
  Nothing -> []

help1 :: Char -> B.ByteString -> Location -> [N]
help1 h rest loc
  | isDigit h = case B.readInt (B.cons h rest) of
      Just (n, r) -> N { location = loc, item = Num n } : (nsFromBs (makeLocation ((length $ show n) + x loc) (y loc)) r)
      Nothing -> []
  | h == '\n' = nsFromBs (makeLocation 0 (1 + y loc)) rest
  | h == '.' = nsFromBs nextCharLoc rest
  | otherwise = N { location = loc, item = Symbol } : (nsFromBs nextCharLoc rest)
        where
        nextCharLoc = makeLocation (1 + x loc) (y loc)

day3part2 :: Solution
day3part2 bs = show . sum $ gearProducts
  where
    ns = nsFromBs2 (makeLocation 0 0) bs
    gears = filter ((== Symbol) . item) ns
    numbers = filter ((/= Symbol) . item) ns
    gearProducts = (flip gearProduct) numbers . location <$> gears

gearProduct :: Location -> [N] -> Int
gearProduct gearLoc ns =
  case getNeighbors gearLoc ns of
    [n1, n2]    -> product $ itemToInt . item <$> [n1, n2]
    _ -> 0

getNeighbors :: Location -> [N] -> [N]
getNeighbors loc ns = do
  n <- ns
  guard $ anyLocationNextTo ([loc]) (nLocations n)
  pure n

nsFromBs2 :: Location -> B.ByteString -> [N]
nsFromBs2 loc bs = case B.uncons bs of
  Just (h, rest) -> help2 h rest loc
  Nothing -> []

help2 :: Char -> B.ByteString -> Location -> [N]
help2 h rest loc
  | isDigit h = case B.readInt (B.cons h rest) of
      Just (n, r) -> N { location = loc, item = Num n } : (nsFromBs (makeLocation ((length $ show n) + x loc) (y loc)) r)
      Nothing -> []
  | h == '\n' = nsFromBs (makeLocation 0 (1 + y loc)) rest
  | h == '*' = N { location = loc, item = Symbol } : (nsFromBs nextCharLoc rest)
  | otherwise = nsFromBs nextCharLoc rest
        where
        nextCharLoc = makeLocation (1 + x loc) (y loc)

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

day4part1 :: Solution
day4part1 bs = show $ sum c
  where
    cardHalves = B.dropWhile (not . isDigit) . B.dropWhile (/= ':') <$> B.lines bs
    numbersPairStrings = mapTuple B.words . B.break (== '|') <$> cardHalves
    a = mapTuple (mapMaybe B.readInt) <$> numbersPairStrings
    pairLists = (mapTuple (fst <$>)) <$> a
    c = (\n -> powerOfTwo $ length n - 1) <$> do
      (ws, ms) <- pairLists
      pure $ do
        w <- ws
        guard $ any (==w) ms
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
    pairLists = (mapTuple (fst <$>)) <$> a
    originalCards = initialCardWins 1 $ length <$> do
      (winNs, myNs) <- pairLists
      pure $ do
        w <- winNs
        guard $ any (==w) myNs
        pure w
    copyCards = winCards originalCards <$> originalCards
    totalCards = sum $ length <$> copyCards

initialCardWins :: Int -> [Int] -> [CardWins]
initialCardWins _ [] = []
initialCardWins i (a:as) = CardWins { cardId = i, wins = a } : (initialCardWins (i+1) as)

data CardWins = CardWins { cardId :: Int, wins :: Int } deriving Show

winCards :: [CardWins] -> CardWins -> [CardWins]
winCards cache src = src : (concat n)
  where
    findCacheWins i = case find ((==i) . cardId) cache of
      Just cw -> wins cw
      Nothing -> 0
    a = (\i -> CardWins { cardId = i, wins = findCacheWins i }) <$> (+ cardId src) <$> [1..wins src]
    n = winCards cache <$> a
