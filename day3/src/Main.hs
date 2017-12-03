module Main where

import qualified Day3

main :: IO ()
main =  do
  line <- read <$> getLine
  print $ Day3.memoryDistance line
