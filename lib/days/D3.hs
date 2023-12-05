module D3 where

import Control.Monad (guard)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Utils (Solution)

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
