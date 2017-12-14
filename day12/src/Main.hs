module Main where

import qualified Day12

main :: IO ()
main =  do
  links <- lines <$> getContents
  let (parta, partb) = Day12.parseCountLinks links
  print parta
  print partb
