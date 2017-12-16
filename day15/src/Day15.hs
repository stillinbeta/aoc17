module Day15 ( generatorA
             , generatorB
             , pickyGeneratorA
             , pickyGeneratorB
             , trailingBytes
             , countPairs
             , countPickyPairs
             ) where

import Data.Bits (xor, (.&.))

generatorA :: Int -> [Int]
generatorA = tail . iterate (generate 16807)

generatorB :: Int -> [Int]
generatorB = tail . iterate (generate 48271)

generate :: Int -> Int -> Int
generate factor x = factor * x `rem` 2147483647

trailingBytes :: Int -> Int -> Bool
trailingBytes x y = (x `xor` y) .&. 0xFFFF == 0

boolToInt :: Bool -> Int
boolToInt False = 0
boolToInt True = 1

countPairs :: Int -> Int -> Int
countPairs genAInit genBInit=
  let pairs = take 40000000 $ zip (generatorA genAInit) (generatorB genBInit) in
     countPairs' pairs

countPairs' :: [(Int, Int)] -> Int
countPairs' = sum . map (boolToInt . uncurry trailingBytes)

pickyGeneratorA :: Int -> [Int]
pickyGeneratorA = filter ((==0) . (`mod`4)) . generatorA

pickyGeneratorB :: Int -> [Int]
pickyGeneratorB = filter ((==0) . (`mod`8)) . generatorB

countPickyPairs :: Int -> Int -> Int
countPickyPairs genAInit genBInit=
  let pairs = take 5000000 $ zip (pickyGeneratorA genAInit) (pickyGeneratorB genBInit) in
    countPairs' pairs
