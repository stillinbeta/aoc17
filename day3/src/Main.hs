module Main where

import qualified Day3

main :: IO ()
main =  do
  i <- read <$> getLine
  print $ Day3.memoryDistance i
  print $ Day3.squareWithValue i
