module Main where

import qualified Day3
import Day3 (Dir (..))
import Test.Hspec

main :: IO ()
main = hspec $
  describe "day3" $ do
    it "should pass part a" $ do
      Day3.spiral 1 `shouldBe` [East]
      Day3.spiral 4 `shouldBe` [East, North, West, West]
      Day3.spiral 12 `shouldBe` [East,  North, West,  West,
                                 South, South, East,  East,
                                 East,  North, North, North]
      Day3.drawGrid 1 `shouldBe` [(0,0)]
      Day3.drawGrid 2 `shouldBe` [(0,0), (1, 0)]
      Day3.drawGrid 4 `shouldBe` [(0,0), (1, 0), (1, 1), (0, 1)]
      Day3.drawGrid 12 `shouldBe` [( 0, 0), ( 1, 0), ( 1, 1), ( 0, 1)
                                       ,(-1, 1), (-1, 0), (-1,-1), ( 0,-1)
                                       ,( 1,-1), ( 2,-1), ( 2, 0), ( 2, 1)]
      Day3.memoryDistance 1 `shouldBe` 0
      Day3.memoryDistance 12 `shouldBe` 3
      Day3.memoryDistance 23 `shouldBe` 2
      Day3.memoryDistance 1024 `shouldBe` 31
    it "should pass part b" $ do
      1 `shouldBe` 1
