module Main where

import qualified Day12
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day12" $ do
    let links = [ "0 <-> 2"
                , "1 <-> 1"
                , "2 <-> 0, 3, 4"
                , "3 <-> 2, 4"
                , "4 <-> 2, 3, 6"
                , "5 <-> 6"
                , "6 <-> 4, 5"
                ]
    it "should pass part a" $ do
      Day12.parseLink "2 <-> 0, 3, 4" `shouldBe` Day12.Link 2 [0, 3, 4]
      Day12.parseLink "0 <-> 2" `shouldBe` Day12.Link 0 [2]
      fst (Day12.parseCountLinks links) `shouldBe` 6
    it "should pass part b" $
      snd (Day12.parseCountLinks links) `shouldBe` 2
