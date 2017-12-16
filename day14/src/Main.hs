module Main where

import qualified Day14

main :: IO ()
main =  do
  line <- getLine
  print $ Day14.usedSquares line
  print $ Day14.regions line
