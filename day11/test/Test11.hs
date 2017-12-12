module Main where

import qualified Day11
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day11" $ do
    it "should pass part a" $ do
      Day11.hexDistance ["ne", "ne", "ne"] `shouldBe` 3
      Day11.hexDistance ["ne", "ne", "sw", "sw"] `shouldBe` 0
      Day11.hexDistance ["ne", "ne", "s", "s"] `shouldBe` 2
      Day11.hexDistance ["se", "sw", "se", "sw", "sw"] `shouldBe` 3
    it "should pass part b" $ do
      pendingWith "part b not unlocked"
