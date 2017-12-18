{-# LANGUAGE BangPatterns #-}

module Day17 ( spinLock
             , spinLock2017
             , spinLock5e7
             , nextIndex
             ) where

spinLock :: Int -> (Int, [Int]) -> Int -> (Int, [Int])
spinLock skip (pos, xs) x =
  let pos' = ((pos + skip) `mod` length xs) + 1 in
  (pos', insert pos' x xs)

insert :: Int -> a -> [a] -> [a]
insert 0 a xs = a:xs
insert i a (x:xs) = x:insert (i-1) a xs
insert _ _ _ = error "list too short"

spinLock2017 :: Int -> Int
spinLock2017 skip = let (pos, xs) =
                          foldl (spinLock skip) (0, [0]) [1..2017] in
  xs !! (pos + 1) `mod` length xs

nextIndex :: Int -> (Int, Int) -> Int -> (Int, Int)
nextIndex skip (pos, !at1) idx = let pos' =  ((pos + skip) `mod` idx) + 1 in
                                  (pos', if pos' == 1 then idx else at1)

spinLock5e7 :: Int -> Int
spinLock5e7 skip = snd $ foldl (nextIndex skip) (0, 1) [1..50000000]
