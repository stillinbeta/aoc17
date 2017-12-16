module Main where

import qualified Day15
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day15" $ do
    it "should pass part a" $ do
      let (gena, genb) = unzip [(    1092455,  430625591 )
                               ,( 1181022009, 1233683848 )
                               ,(  245556042, 1431495498 )
                               ,( 1744312007,  137874439 )
                               ,( 1352636452,  285222916 )
                               ]
      take 5 (Day15.generatorA 65) `shouldBe` gena
      take 5 (Day15.generatorB 8921) `shouldBe` genb

      (gena !! 0) `Day15.trailingBytes` (genb !! 0) `shouldBe` False
      (gena !! 1) `Day15.trailingBytes` (genb !! 1) `shouldBe` False
      (gena !! 2) `Day15.trailingBytes` (genb !! 2) `shouldBe` True
      (gena !! 3) `Day15.trailingBytes` (genb !! 3) `shouldBe` False
      (gena !! 4) `Day15.trailingBytes` (genb !! 4) `shouldBe` False

      Day15.countPairs 65 8921 `shouldBe` 588
    it "should pass part b" $ do
      let (gena, genb) = unzip [(1352636452,  1233683848)
                               ,(1992081072,  862516352)
                               ,(530830436,   1159784568)
                               ,(1980017072,  1616057672)
                               ,(740335192,   412269392)
                               ]
      take 5 (Day15.pickyGeneratorA 65) `shouldBe` gena
      take 5 (Day15.pickyGeneratorB 8921) `shouldBe` genb
      Day15.countPickyPairs 65 8921 `shouldBe` 309
