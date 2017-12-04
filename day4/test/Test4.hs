module Main where

import qualified Day4
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day4" $ do
    it "should pass part a" $ do
      let passwords = ["aa bb cc dd ee"
                      ,"aa bb cc dd aa"
                      ,"aa bb cc dd aaa"
                      ]
      Day4.isValid (passwords !! 0) `shouldBe` True
      Day4.isValid (passwords !! 1) `shouldBe` False
      Day4.isValid (passwords !! 2) `shouldBe` True
      Day4.countValid passwords `shouldBe` 2
    it "should pass part b" $ do
      let passwords =["abcde fghij"
                     ,"abcde xyz ecdab"
                     ,"a ab abc abd abf abj"
                     ,"iiii oiii ooii oooi oooo"
                     ,"oiii ioii iioi iiio"
                     ]
      Day4.isValidAnagrams (passwords !! 0) `shouldBe` True
      Day4.isValidAnagrams (passwords !! 1) `shouldBe` False
      Day4.isValidAnagrams (passwords !! 2) `shouldBe` True
      Day4.isValidAnagrams (passwords !! 3) `shouldBe` True
      Day4.isValidAnagrams (passwords !! 4) `shouldBe` False
      Day4.countValidAnagarms passwords `shouldBe` 3
