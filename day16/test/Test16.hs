module Main where

import qualified Day16
import Test.Hspec

import qualified Data.Vector as V

main :: IO ()
main = hspec $ do
  describe "day16" $ do
    it "should pass part a" $ do
      let v = Day16.makeVector 5
      let v2 = V.fromList "eabcd"
      Day16.doInstruction v (Day16.Spin 1) `shouldBe` v2
      let v3 = V.fromList "eabdc"
      Day16.doInstruction v2 (Day16.Exchange 3 4) `shouldBe` v3
      let v4 = V.fromList "baedc"
      Day16.doInstruction v3 (Day16.Partner 'e' 'b') `shouldBe` v4

      Day16.parse "s12" `shouldBe` Day16.Spin 12
      Day16.parse "x17/15" `shouldBe` Day16.Exchange 17 15
      Day16.parse "pe/d" `shouldBe` Day16.Partner 'e' 'd'
      Day16.dance' 5 [ "s1", "x3/4", "pe/b"] `shouldBe` "baedc"
    it "should pass part b" $ do
      pendingWith "part b not unlocked"
