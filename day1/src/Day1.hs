module Day1 (
  sumDuplicates
, sumMiddleDuplicates
  ) where

sumDuplicates :: [Int] -> Int
sumDuplicates [] = 0
sumDuplicates (x:xs) = sumDuplicates' x (x:xs)

sumDuplicates' :: Int -> [Int] -> Int
sumDuplicates' f (x1:x2:xs) =
  let add = if x1 == x2 then x1 else 0 in
  add + sumDuplicates' f (x2:xs)
sumDuplicates' f (x:_) =
  if f == x then x else 0
sumDuplicates' _ _ = 0

sumMiddleDuplicates :: [Int] -> Int
sumMiddleDuplicates xs =
  let (x1, x2) = splitAt (length xs `div` 2) xs in
  sumMiddleDuplicates' xs (x2 ++ x1)

sumMiddleDuplicates' :: [Int] -> [Int] -> Int
sumMiddleDuplicates' [] _ = 0
sumMiddleDuplicates' _ [] = 0
sumMiddleDuplicates' (x1:x1s) (x2:x2s) =
  let add = if x1 == x2 then x1 else 0 in
    add + sumMiddleDuplicates' x1s x2s
