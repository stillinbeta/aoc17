module Main where

import Day8
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day8" $ do
    let cmds = [ "b inc 5 if a > 1"
               , "a inc 1 if b < 5"
               , "c dec -10 if a >= 1"
               , "c inc -20 if c == 10"
               ]
    it "should pass part a" $ do
      parseCmd (cmds !! 0) `shouldBe` Cmd "b" 5 "a" GreaterThan 1
      parseCmd (cmds !! 1) `shouldBe` Cmd "a" 1 "b" LessThan 5
      parseCmd (cmds !! 2) `shouldBe` Cmd "c" 10 "a" GreaterThanOrEqual 1
      parseCmd (cmds !! 3) `shouldBe` Cmd "c" (-20) "c" EqualTo 10
      fst (largestVariable cmds) `shouldBe` 1
    it "should pass part b" $ do
      snd (largestVariable cmds) `shouldBe` 10
