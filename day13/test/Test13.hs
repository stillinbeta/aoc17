module Main where

import qualified Day13
import Test.Hspec

main :: IO ()
main = hspec $ do
  let input = [ "0: 3"
              , "1: 2"
              , "4: 4"
              , "6: 4"
              ]
  describe "day13" $ do
    it "should pass part a" $ do
      Day13.score 0 3 `shouldBe` 0
      Day13.score 1 2 `shouldBe` 0
      Day13.score 4 4 `shouldBe` 0
      Day13.score 6 4 `shouldBe` 24
      Day13.scoreAll input `shouldBe` 24
    it "should pass part b" $ do
      Day13.lowestScore input `shouldBe` 10
