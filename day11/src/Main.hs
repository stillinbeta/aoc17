module Main where

import Data.List.Split (splitOn)
import qualified Day11

main :: IO ()
main =  do
  dirs <- splitOn "," <$> getLine
  print $ Day11.hexDistance dirs
  print $ Day11.maxDistance dirs
