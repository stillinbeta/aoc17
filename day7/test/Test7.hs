module Main where

import qualified Day7
import Test.Hspec

input = [ "pbga (66)"
        , "xhth (57)"
        , "ebii (61)"
        , "havc (66)"
        , "ktlj (57)"
        , "fwft (72) -> ktlj, cntj, xhth"
        , "qoyq (66)"
        , "padx (45) -> pbga, havc, qoyq"
        , "tknk (41) -> ugml, padx, fwft"
        , "jptl (61)"
        , "ugml (68) -> gyxo, ebii, jptl"
        , "gyxo (61)"
        , "cntj (57)"
        ]

main :: IO ()
main = hspec $ do
  describe "day7" $ do
    it "should pass part a" $ do
      let expected = Day7.Program 72 "fwft" $ map Day7.ProgramPointer ["ktlj", "cntj", "xhth"]
      Day7.parseLine "fwft (72) -> ktlj, cntj, xhth" `shouldBe` expected
      Day7.getKey (Day7.rootProgram input) `shouldBe` "tknk"
    it "should pass part b" $ do
      let root = Day7.rootProgram input
      Day7.balanceNum root `shouldBe` 60
