module Main where

import qualified Day14
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day14" $ do
    let input = "flqrgnkx"
    it "should pass part a" $ do
      [[Day14.squareUsed input row col | col <- [0..7]] | row <- [0..3]]
        `shouldBe` [ [  True,  True, False,  True, False,  True, False, False ]
                   , [ False,  True, False,  True, False,  True, False,  True ]
                   , [ False, False, False, False,  True, False,  True, False ]
                   , [  True, False,  True, False,  True,  True, False,  True ]
                   ]
      Day14.usedSquares input `shouldBe` 8108

    it "should pass part b" $ do
      Day14.regions input `shouldBe` 1242
