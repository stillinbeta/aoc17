module Main where

import qualified Day19
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day19" $ do
    let labyrinth = [ "      |          "
                    , "      |  +--+    "
                    , "      A  |  C    "
                    , "  F---|----E|--+ "
                    , "      |  |  |  D "
                    , "      +B-+  +--+ "
                    ]
    it "should pass part a" $
      snd (Day19.followLabyrinth labyrinth) `shouldBe` "ABCDEF"
    it "should pass part b" $
      fst (Day19.followLabyrinth labyrinth) `shouldBe` 38
