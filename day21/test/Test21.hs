module Main where

import qualified Day21
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day21" $ do
    it "should pass part a" $ do
      let p1 = [[  True,  True, False ]
               ,[ False, False, False ]
               ,[ False,  True, False ]]
      let rotated = [[ False, False,  True ]
                    ,[  True, False,  True ]
                    ,[ False, False, False ]]
      let rotated2 = [[ False,  True, False ]
                     ,[ False, False, False ]
                     ,[ False,  True,  True ]]
      let reflected = [[ False,  True,  True ]
                      ,[ False, False, False ]
                      ,[ False,  True, False ]]
      Day21.rotate p1 `shouldBe` rotated
      Day21.rotate rotated `shouldBe` rotated2
      Day21.reflect p1 `shouldBe` reflected
      let rules = ["../.# => ##./#../..."
                  ,".#./..#/### => #..#/..../..../#..#"
                  ]
      Day21.parse (rules !! 0) `shouldBe` ([[False, False]
                                           ,[False,  True]],
                                           [[ True,  True, False]
                                           ,[ True, False, False]
                                           ,[False, False, False]])
      Day21.parse (rules !! 1) `shouldBe` ([[False,  True, False]
                                           ,[False, False,  True]
                                           ,[ True,  True,  True]],
                                           [[ True, False, False,  True]
                                           ,[False, False, False, False]
                                           ,[False, False, False, False]
                                           ,[ True, False, False,  True]])
      Day21.enhance 2 rules `shouldBe` 12
    it "should pass part b" $ do
      pendingWith "part b not unlocked"
