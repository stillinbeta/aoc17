module Main where

import qualified Day8

main :: IO ()
main =  do
  cmds <- lines <$> getContents
  let (parta, partb) = Day8.largestVariable cmds
  print parta
  print partb
