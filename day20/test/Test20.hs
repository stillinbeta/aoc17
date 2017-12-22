module Main where

import Day20
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "day20" $ do
    it "should pass part a" $ do
      let vectors = [ "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
                    , "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>"
                    ]
      map Day20.parseParticle vectors `shouldBe` [ Particle (Vector 3 0 0)
                                                            (Vector 2 0 0)
                                                            (Vector (-1) 0 0 )
                                                 , Particle (Vector 4 0 0)
                                                            (Vector 0 0 0)
                                                            (Vector (-2) 0 0 )
                                                 ]

      closestParticle vectors `shouldBe` 0
      let vectors1 = [ "p=<3,0,0>, v=<2,0,0>, a=<-1,20,4>"
                    , "p=<4,0,0>, v=<0,0,0>, a=<-2,0,5>"
                    ]
      closestParticle vectors1 `shouldBe` 1
    it "should pass part b" $ do
      let vectors = [ Particle (Vector 3 0 0)
                               (Vector 2 0 0)
                               (Vector (-1) 0 0)
                    , Particle (Vector 4 0 0)
                               (Vector 1 0 0)
                               (Vector (-1) 0 0)
                    , Particle (Vector 4 0 0)
                               (Vector 0 0 0)
                               (Vector (-1) 0 0)
                    , Particle (Vector 3 0 0)
                               (Vector (-1) 0 0)
                               (Vector (-1) 0 0)
                    ]
      take 4 (iterate tick (head vectors)) `shouldBe` vectors
      let vectors2 = [ "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>"
                     , "p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>"
                     , "p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>"
                     , "p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>"
                     ]
      survivingParticles vectors2 `shouldBe` 1
