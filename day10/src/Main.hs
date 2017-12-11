module Main where


import qualified Day10

import Data.List.Split (splitOn)

main :: IO ()
main =  do
  lengths <- map read . splitOn "," <$> getLine
  print $ Day10.knotTie 256 lengths
