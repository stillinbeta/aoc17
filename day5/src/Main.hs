module Main where

import qualified Day5

main :: IO ()
main =  do
  instrs <- map read . lines <$> getContents
  print $ Day5.jumpsToEscapeA instrs
  print $ Day5.jumpsToEscapeB instrs
