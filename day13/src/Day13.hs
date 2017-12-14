module Day13 ( score
             , scoreAll
             , lowestScore
             ) where

import Data.Maybe (fromJust)
import Data.List (find)
import Data.List.Split (splitOn)

lowestScore :: [String] -> Int
lowestScore strs = let fw = map parse strs in
  fromJust $ find (checkRun fw) [0..]

checkRun :: [(Int, Int)] -> Int -> Bool
checkRun fws delay = all (\(pos, size) -> not $ areCaught (pos + delay) size) fws

parse :: String -> (Int, Int)
parse s = let [pos, size] = splitOn ": " s in
            (read pos, read size)

scoreAll :: [String] -> Int
scoreAll = sum . map (uncurry score . parse)

areCaught :: Int -> Int -> Bool
areCaught pos size = pos `mod` (size + (size - 2)) == 0

score :: Int -> Int -> Int
score pos size = if areCaught pos size
                 then pos * size
                 else 0
