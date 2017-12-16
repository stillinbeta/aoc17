module Main where

import qualified Day15

main :: IO ()
main =  do
  [genA, genB] <- map (last . words) . lines <$> getContents
  let (genA', genB') = (read genA, read genB)
  print $ Day15.countPairs genA' genB'
  print $ Day15.countPickyPairs genA' genB'
