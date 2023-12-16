module D1 where

import qualified Data.ByteString.Lazy.Char8 as B
import Utils (Solution)
import Data.Maybe (mapMaybe, fromJust)
import Data.Char (isDigit)
import Data.List (find)

day1part1 :: Solution
day1part1 = show . day1Solution part1Strategy

day1Solution :: Day1Strategy -> B.ByteString -> Int
day1Solution f bs = sum $ mapMaybe f (B.lines bs)

type Day1Strategy = B.ByteString -> Maybe Int

part1Strategy :: Day1Strategy
part1Strategy bs =
  let
    firstAndLastDigits :: [Char]
    firstAndLastDigits = mapMaybe (B.find isDigit) [ bs, B.reverse bs]
  in
    do
      (int, _) <- B.readInt $ B.pack firstAndLastDigits
      pure int

day1part2 :: Solution
day1part2 = show . day1Solution part2Strategy

part2Strategy :: Day1Strategy
part2Strategy bs = do
      tup <- findFirstAndLastDigit bs
      combineDigits tup

combineDigits :: (Char, Char) -> Maybe Int
combineDigits (c1, c2) = case (B.readInt . B.pack) [c1, c2] of
                           Just (n, "") -> Just n
                           _            -> Nothing

findFirstAndLastDigit :: B.ByteString -> Maybe (Char, Char)
findFirstAndLastDigit bs =
  let
    firstD = findFirstDigit bs             prefixes                 prefixToDigit
    lastD  = findFirstDigit (B.reverse bs) (B.reverse <$> prefixes) (prefixToDigit . B.reverse)
    -- Note:
    -- Finding the last digit is just finding the first digit for the reversed ByteString,
    -- with reversed versions of prefixes to match and after match they need to be reversed to make digits.
  in
    do
      f <- firstD
      l <- lastD
      pure (f, l)

-- 1. Try to match a digit in the head,
--    1a. If head is digit return the head.
--    1b. 2. Else try to match any of the given prefixes,
--          a. If found, make it a digit with the given prefixToDigit function.
--          b. Else start from 1. without the head.
findFirstDigit :: B.ByteString -> [B.ByteString] -> (B.ByteString -> Maybe Char) -> Maybe Char
findFirstDigit "" _    _          = Nothing
findFirstDigit s  pfxs pfxToDigit =
  let
    (head', tail') = (fromJust . B.uncons) s
  in
    if isDigit head'
      then Just head'
      else case find (`B.isPrefixOf` s) pfxs of
             Just prefix -> pfxToDigit prefix
             Nothing     -> findFirstDigit tail' pfxs pfxToDigit

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

prefixToDigit :: B.ByteString -> Maybe Char
prefixToDigit "one" = Just '1'
prefixToDigit "two" = Just '2'
prefixToDigit "three" = Just '3'
prefixToDigit "four" = Just '4'
prefixToDigit "five" = Just '5'
prefixToDigit "six" = Just '6'
prefixToDigit "seven" = Just '7'
prefixToDigit "eight" = Just '8'
prefixToDigit "nine" = Just '9'
prefixToDigit _ = Nothing
