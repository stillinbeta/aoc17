module Main where

import qualified Day18

main :: IO ()
main =  do
  instrs <- lines <$> getContents
  print $ Day18.duet instrs
