module Main where

import qualified Day20

main :: IO ()
main =  do
  particles <- lines <$> getContents
  print $ Day20.closestParticle particles
  print $ Day20.survivingParticles particles
