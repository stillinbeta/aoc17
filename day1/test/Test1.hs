module Main where

import qualified Day1
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day1" $ do
    it "should pass part a" $ do
      Day1.sumDuplicates [1, 1, 2, 2] `shouldBe` 3
      Day1.sumDuplicates [1, 1, 1, 1] `shouldBe` 4
      Day1.sumDuplicates [1, 2, 3, 4] `shouldBe` 0
      Day1.sumDuplicates [1, 2, 3, 4] `shouldBe` 0
      Day1.sumDuplicates [9, 1, 2, 1, 2, 1, 2, 9] `shouldBe` 9
    it "should pass part b" $ do
      Day1.sumMiddleDuplicates [1, 2, 1, 2] `shouldBe` 6
      Day1.sumMiddleDuplicates [1, 2, 2, 1] `shouldBe` 0
      Day1.sumMiddleDuplicates [1, 2, 3, 4, 2, 5] `shouldBe` 4
      Day1.sumMiddleDuplicates [1, 2, 3, 1, 2, 3] `shouldBe` 12
      Day1.sumMiddleDuplicates [1, 2, 1, 3, 1, 4, 1, 5] `shouldBe` 4
