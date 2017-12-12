module Main where


import qualified Day10

import Data.List.Split (splitOn)

main :: IO ()
main =  do
  line <- getLine
  let lengths = map read . splitOn "," $ line
  print $ Day10.knotTie 256 lengths
  print $ Day10.knotHash line
