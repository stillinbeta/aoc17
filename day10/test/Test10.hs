module Main where

import qualified Day10
import Test.Hspec

import Data.Vector.Unboxed (fromList)

main :: IO ()
main = hspec $ do
  describe "day10" $ do
    it "should pass part a" $ do
      let v1 = Day10.KnotTie (fromList [0..4]) 0 0
      let v2 = Day10.KnotTie (fromList [2, 1, 0, 3, 4]) 1 3
      Day10.reverseSection v1 3 `shouldBe` v2
      let v3 = Day10.KnotTie (fromList [4, 3, 0, 1, 2]) 2 3
      Day10.reverseSection v2 4 `shouldBe` v3
      let v4 = Day10.KnotTie (fromList [4, 3, 0, 1, 2]) 3 1
      Day10.reverseSection v3 1 `shouldBe` v4
      let v5 = Day10.KnotTie (fromList [3, 4, 2, 1, 0]) 4 4
      Day10.reverseSection v4 5 `shouldBe` v5
      Day10.knotTie 5 [3, 4, 1, 5] `shouldBe` 12
    it "should pass part b" $ do
      Day10.knotHash "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
      Day10.knotHash "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
      Day10.knotHash "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
      Day10.knotHash "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
