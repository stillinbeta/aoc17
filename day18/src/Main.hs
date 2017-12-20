module Main where

import qualified Day18

main :: IO ()
main =  do
  instrs <- lines <$> getContents
  print $ Day18.duetA instrs
  print $ Day18.duetB instrs
