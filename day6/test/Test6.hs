module Main where

import qualified Day6
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day6" $ do
    let mem0 = Day6.toIMap [0, 2, 7, 0]
    it "should pass part a" $ do
      Day6.findMaxIdx mem0 `shouldBe` (2, 7)
      let mem1 = Day6.toIMap [2, 4, 1, 2]
      Day6.redistribute mem0 `shouldBe` mem1
      let mem2 = Day6.toIMap [3, 1, 2, 3]
      Day6.redistribute mem1 `shouldBe` mem2
      let mem3 = Day6.toIMap [0, 2, 3, 4]
      Day6.redistribute mem2 `shouldBe` mem3
      let mem4 = Day6.toIMap [1, 3, 4, 1]
      Day6.redistribute mem3 `shouldBe` mem4
      Day6.redistribute mem4 `shouldBe` mem1
      fst (Day6.cyclesTilLoop [0, 2, 7, 0]) `shouldBe` 5
    it "should pass part b" $ do
      snd (Day6.cyclesTilLoop [0, 2, 7, 0]) `shouldBe` 4
