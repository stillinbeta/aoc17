module Main where

import qualified Day16

import Data.List.Split (splitOn)

main :: IO ()
main =  do
  instrs <- splitOn "," <$> getLine
  print $ Day16.dance instrs
  print $ Day16.danceForever instrs
