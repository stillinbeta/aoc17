module Main where

import qualified Day16

import Data.List.Split (splitOn)

main :: IO ()
main =  do
  instrs <- splitOn "," <$> getLine
  putStrLn $ Day16.dance instrs
  putStrLn $ Day16.danceForever instrs
