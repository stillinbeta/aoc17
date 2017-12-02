module Main where

import qualified Day2
import Test.Hspec

main :: IO ()
main = hspec $
  describe "day2" $ do
    it "should pass part a" $ do
      let sheet = [[5, 1, 9, 5]
                  ,[7, 5, 3]
                  ,[2, 4, 6, 8]]
      Day2.checkSum sheet `shouldBe` 18
    it "should pass part b" $ do
      let sheet = [[5, 9, 2, 8]
                  ,[9, 4, 7, 3]
                  ,[3, 8, 6, 5]]
      Day2.rowQuotient (sheet !! 0) `shouldBe` 4
      Day2.rowQuotient (sheet !! 1) `shouldBe` 3
      Day2.rowQuotient (sheet !! 2) `shouldBe` 2
      Day2.divideSum sheet `shouldBe` 9
