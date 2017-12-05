module Main where

import qualified Day5
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day5" $ do
    it "should pass part a" $ do
      -- [0] 3 0 1 -3
      let orig = Day5.InstructionList [] [3, 0, 1, -3] 0
      -- [1] 3 0 1 -3
      let res1 = Day5.InstructionList [] [3, 0, 1, -3] 1
      Day5.nextInstructionPartA orig `shouldBe` Just res1
      -- 2 [3] 0 1 -3
      let res2 = Day5.InstructionList [2] [0, 1, -3] 3
      Day5.nextInstructionPartA res1 `shouldBe` Just res2
      -- 2 4 0 1 [-3]
      let res3 = Day5.InstructionList [1, 0, 4, 2] [] (-3)
      Day5.nextInstructionPartA res2 `shouldBe` Just res3
      -- 2 [4] 0 1 -2
      let res4 = Day5.InstructionList [2] [0, 1, -2] 4
      Day5.nextInstructionPartA res3 `shouldBe` Just res4
      Day5.nextInstructionPartA res4 `shouldBe` Nothing
      Day5.jumpsToEscapeA [0, 3, 0, 1, -3] `shouldBe` 5
    it "should pass part b" $ do
      Day5.jumpsToEscapeB [0, 3, 0, 1, -3] `shouldBe` 10
