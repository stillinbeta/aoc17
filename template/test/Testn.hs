module Main where

import qualified Dayn
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day1" $ do
    it "should pass part a" $ do
      1 `shouldBe` 1
    it "should pass part b" $ do
      1 `shouldBe` 1
