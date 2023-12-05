module Utils where

import qualified Data.ByteString.Lazy.Char8 as B

type Input = B.ByteString
type Output = String
type Solution = Input -> Output
