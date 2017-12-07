module Main where

import qualified Day6

main :: IO ()
main =  do
  line <- map read . words <$> getLine
  let (parta, partb) = Day6.cyclesTilLoop line
  print parta
  print partb
