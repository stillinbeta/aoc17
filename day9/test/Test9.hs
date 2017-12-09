module Main where

import Day9
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day9" $ do
    it "should pass part a" $ do
      (filterGroups . parseStream) "{}" `shouldBe` [Group []]
      (filterGroups . parseStream) "{{{}}}" `shouldBe` [Group [Group [Group []]]]
      (filterGroups . parseStream) "{{{}}}" `shouldBe` [Group [Group [Group []]]]
      (filterGroups . parseStream) "{{{},{},{{}}}}" `shouldBe` [Group [Group [Group [], Group[], Group[Group []]]]]
      (filterGroups . parseStream) "{<{},{},{{}}>}" `shouldBe` [Group []]
      (filterGroups . parseStream) "{<a>,<a>,<a>,<a>}" `shouldBe` [Group []]
      (filterGroups . parseStream) "{{<!>},{<!>},{<!>},{<a>}}" `shouldBe` [Group [Group []]]

      let streams = [ "{}"
                    , "{{{}}}"
                    , "{{},{}}"
                    , "{{{},{},{{}}}}"
                    , "{<a>,<a>,<a>,<a>}"
                    , "{{<ab>},{<ab>},{<ab>},{<ab>}}"
                    , "{{<!!>},{<!!>},{<!!>},{<!!>}}"
                    , "{{<a!>},{<a!>},{<a!>},{<ab>}}"
                    ]
      countGroups (streams !! 0) `shouldBe` 1
      countGroups (streams !! 1) `shouldBe` 6
      countGroups (streams !! 2) `shouldBe` 5
      countGroups (streams !! 3) `shouldBe` 16
      countGroups (streams !! 4) `shouldBe` 1
      countGroups (streams !! 5) `shouldBe` 9
      countGroups (streams !! 6) `shouldBe` 9
      countGroups (streams !! 7) `shouldBe` 3
      fst (countAllGroups streams) `shouldBe` 50

    it "should pass part b" $ do
      countGarbage "{<>}" `shouldBe` 0
      countGarbage "{<random characters>}" `shouldBe` 17
      countGarbage "{<<<<>}" `shouldBe` 3
      countGarbage "{<{!>}>}" `shouldBe` 2
      countGarbage "{<!!>}" `shouldBe` 0
      countGarbage "{<!!!>>}" `shouldBe` 0
      countGarbage "{<{o\"i!a,<{i<a>}" `shouldBe` 10
