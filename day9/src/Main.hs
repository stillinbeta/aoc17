module Main where

import qualified Day9

main :: IO ()
main =  do
  groups <- lines <$> getContents
  let (parta, partb) = Day9.countAllGroups groups
  print parta
  print partb
