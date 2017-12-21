module Main where

import qualified Day19

main :: IO ()
main =  do
  labyrinth <- lines <$> getContents
  let (partb, parta) = Day19.followLabyrinth labyrinth
  putStrLn parta
  print partb
