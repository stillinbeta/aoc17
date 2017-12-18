module Main where

import qualified Day17

main :: IO ()
main =  do
  skip <- read <$> getLine
  print $ Day17.spinLock2017 skip
  print $ Day17.spinLock5e7 skip
