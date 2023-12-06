module Main where

import Solution
import Test.HUnit
import Data.List (break, tail)
import qualified System.Exit as Exit

createTestCase :: String -> String -> IO (Test)
createTestCase testId expected = do
  testCase <- getSolution [testId]
  pure $ TestLabel label (TestCase (assertEqual text expected testCase))
    where
      (daypart, partpart) = break (== '.') testId
      label = "Day" <> daypart <> "Part" <> (tail partpart)
      text = "should return " <> expected

listToTests :: [(String, String)] -> IO [Test]
listToTests ts = sequence $ (\t -> createTestCase (fst t) (snd t)) <$> ts

tests :: IO (Test)
tests = do
  ts <- listToTests [
                    -- Day1
                      ("1.1e", "142")
                    , ("1.1" , "54644")
                    , ("1.2e", "281")
                    , ("1.2" , "53348")

                    -- Day2
                    , ("2.1e", "8")
                    , ("2.1" , "2486")
                    , ("2.2e", "2286")
                    , ("2.2" , "87984")

                    -- Day3
                    , ("3.1e", "4361")
                    , ("3.1" , "543867")
                    , ("3.2e", "467835")
                    , ("3.2" , "79613331")

                    -- Day4
                    , ("4.1e", "13")
                    , ("4.1" , "22193")
                    , ("4.2e", "30")
                    , ("4.2" , "5625994")

                    -- Day5
                    , ("5.1e", "35")
                    , ("5.1", "389056265")
                    ]
  pure $ TestList ts

main :: IO ()
main = do
  t <- tests
  result <- runTestTT t
  if (failures result) > 0 then Exit.exitFailure else Exit.exitSuccess
