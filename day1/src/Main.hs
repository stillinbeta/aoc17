module Main where

import qualified Day1

main :: IO ()
main =  do
  line <- getLine
  let line' = map (read . (:"")) line
  print $ Day1.sumDuplicates line'
  print $ Day1.sumMiddleDuplicates line'
