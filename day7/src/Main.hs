module Main where

import qualified Day7

main :: IO ()
main =  do
  programs <- lines <$> getContents
  let root = Day7.rootProgram programs
  print $ Day7.getKey root
  print $ Day7.balanceNum root
