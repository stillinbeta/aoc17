module Main where

import qualified Day13

main :: IO ()
main =  do
  lines <- lines <$> getContents
  print $ Day13.scoreAll lines
  print $ Day13.lowestScore lines
