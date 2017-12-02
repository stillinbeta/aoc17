module Day2 (checkSum
            ,divideSum
            ,rowQuotient
            ) where

import Data.List (find)
import Data.Maybe (catMaybes)

checkSum :: [[Int]] -> Int
checkSum xs =
  let maxMins = map maxMin xs in
    sum $ map (abs . uncurry (-)) maxMins

maxMin :: [Int] -> (Int, Int)
maxMin [] = (0, 0) -- Not strictly correct, but better than `error`
maxMin [x] = (x, x)
maxMin (x:xs) =
  let (mx, mi) = maxMin xs in
    (max mx x, min mi x)

divideSum :: [[Int]] -> Int
divideSum = sum . map rowQuotient

rowQuotient :: [Int] -> Int
rowQuotient xs = rowQuotient' xs xs

rowQuotient' :: [Int] -> [Int] -> Int
rowQuotient' _ [] = error "no results found"
rowQuotient' l (x:xs) =
  let div1 = (`div`x) <$> find ((==0) . (`mod`x)) l
      div2 = (x`div`) <$> find ((==0) . (x`mod`)) l in
    case filter (>1) $ catMaybes [div1, div2] of
      (q:_) -> q
      [] -> rowQuotient' l xs
