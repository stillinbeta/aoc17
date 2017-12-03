module Main where

import qualified Day3
import Day3 (Dir (..))
import Test.Hspec

main :: IO ()
main = hspec $
  describe "day3" $ do
    it "should pass part a" $ do
      take 1 Day3.spiral `shouldBe` [East]
      take 4 Day3.spiral `shouldBe` [East, North, West, West]
      take 12 Day3.spiral `shouldBe` [East,  North, West,  West,
                                      South, South, East,  East,
                                      East,  North, North, North]
      take 1 Day3.drawGrid `shouldBe` [(0,0)]
      take 2 Day3.drawGrid `shouldBe` [(0,0), (1, 0)]
      take 4 Day3.drawGrid `shouldBe` [(0,0), (1, 0), (1, 1), (0, 1)]
      take 12 Day3.drawGrid `shouldBe` [( 0, 0), ( 1, 0), ( 1, 1), ( 0, 1)
                                       ,(-1, 1), (-1, 0), (-1,-1), ( 0,-1)
                                       ,( 1,-1), ( 2,-1), ( 2, 0), ( 2, 1)]
      Day3.memoryDistance 1 `shouldBe` 0
      Day3.memoryDistance 12 `shouldBe` 3
      Day3.memoryDistance 23 `shouldBe` 2
      Day3.memoryDistance 1024 `shouldBe` 31
    it "should pass part b" $ do
      Day3.fillSquares !! 0 `shouldBe` 1
      Day3.fillSquares !! 1 `shouldBe` 1
      Day3.fillSquares !! 2 `shouldBe` 2
      Day3.fillSquares !! 3 `shouldBe` 4
      Day3.fillSquares !! 4 `shouldBe` 5
