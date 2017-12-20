module Main where

import Day18
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day18" $ do
    it "should pass part a" $ do
      let instrs = [ "set a 1"
                   , "add a 2"
                   , "mul a a"
                   , "mod a 5"
                   , "snd a"
                   , "set a 0"
                   , "rcv a"
                   , "jgz a -1"
                   , "set a 1"
                   , "jgz a -2"
                   ]
      let instrs' = [ Set 'a' (Num 1)
                    , Add 'a' (Num 2)
                    , Mul 'a' (Register 'a')
                    , Mod 'a' (Num 5)
                    , Snd (Register 'a')
                    , Set 'a' (Num 0)
                    , Rcv (Register 'a')
                    , Jgz (Register 'a') (Num (-1))
                    , Set 'a' (Num 1)
                    , Jgz (Register 'a') (Num (-2))
                    ]
      map parse instrs `shouldBe` instrs'
      duetA instrs `shouldBe` 4
    it "should pass part b" $ do
      let instrs = [ "snd 1"
                   , "snd 2"
                   , "snd p"
                   , "rcv a"
                   , "rcv b"
                   , "rcv c"
                   , "rcv d"
                   ]
      duetB instrs `shouldBe` 3
