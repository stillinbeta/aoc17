module Main where

import qualified Day2

main :: IO ()
main =  do
  sheet <- (((read <$>) .  words <$>) . lines) <$> getContents
  print $ Day2.checkSum sheet
  print $ Day2.divideSum sheet
