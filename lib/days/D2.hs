module D2 where

import qualified Data.ByteString.Lazy.Char8 as B
import Data.Char (isDigit)
import Utils (Solution)

day2part1 :: Solution
day2part1 bs = show $ sum . map (identifier) $ filter (withinCapacity capacityBag . maximumBag) $ parseGame <$> B.lines bs
  where
    capacityBag = Bag { red = 12
                      , green = 13
                      , blue = 14
                      }

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
