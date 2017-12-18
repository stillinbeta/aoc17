module Main where

import qualified Day17
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day17" $ do
    let input = 3
    it "should pass part a" $ do
      let a0 = (0, [0])
      let a1 = (1, [0, 1])
      let a2 = (1, [0, 2, 1])
      let a3 = (2, [0, 2, 3, 1])
      let a4 = (2, [0, 2, 4, 3, 1])
      Day17.spinLock input a0 1 `shouldBe` a1
      Day17.spinLock input a1 2 `shouldBe` a2
      Day17.spinLock input a2 3 `shouldBe` a3
      Day17.spinLock input a3 4 `shouldBe` a4
      Day17.spinLock2017 input `shouldBe` 638
    it "should pass part b" $ do
      Day17.nextIndex 3 (1, 1) 2 `shouldBe` (1, 2)
