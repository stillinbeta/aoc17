module Main where

import qualified Day4

main :: IO ()
main =  do
  pps <- lines <$> getContents
  print $ Day4.countValid pps
  print $ Day4.countValidAnagarms pps
