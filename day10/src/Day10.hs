module Day10 ( reverseSection
             , KnotTie(..)
             , knotTie
             , knotHash
             ) where

import Data.Char (ord)
import Data.Bits (xor)
import Text.Printf (printf)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

data KnotTie = KnotTie { vector :: V.Vector Int
                       , skipSize :: Int
                       , currentPosition :: Int
                       } deriving (Eq, Show)

iv :: [Int]
iv = [17, 31, 73, 47, 23]

nTimes :: Int -> (a -> a) -> a -> a
nTimes 0 _ x = x
nTimes i f x = nTimes (i - 1) f (f x)

knotHash :: String -> String
knotHash str =
  let lengths = map ord str ++ iv
      kt = KnotTie (V.enumFromN 0 256) 0 0
      kt' = nTimes 64 (knotIteration lengths) kt
      dense = denseHash $ vector kt'
  in toHex $ V.toList dense

toHex :: [Int] -> String
toHex = foldr ((++) . printf "%02x") ""

denseHash :: V.Vector Int -> V.Vector Int
denseHash v
  | V.null v = V.empty
  | otherwise =
    let (x, xs) = V.splitAt 16 v in
      V.foldl1 xor x `V.cons` denseHash xs


knotIteration :: [Int] -> KnotTie ->KnotTie
knotIteration lengths kt = foldl reverseSection kt lengths

knotTie :: Int -> [Int] -> Int
knotTie size lengths =
  let v = vector $ knotIteration lengths $ KnotTie (V.enumFromN 0 size) 0 0 in
    (v V.! 0) * (v V.! 1)

reverseSection :: KnotTie -> Int -> KnotTie
reverseSection  KnotTie { vector = v
                        , skipSize = skip
                        , currentPosition = from
                        } revLen =
  let to = (from + revLen) - 1
      vSize =  V.length v
      -- [(from, to), (from+1, to-1), from+2, to-2] etc
      swaps = take (revLen `div` 2) $ zip (iterate (+1) from)
                                          -- grumble grumble ambiguous -
                                          (iterate (subtract 1) to)
      v' = V.modify (\vm -> mapM_ (uncurry (swap vm)) swaps) v
      pos' = (from + skip + revLen) `mod` vSize in

    KnotTie { vector = v', skipSize = skip + 1, currentPosition = pos'}

-- Type is too much work tbh
swap v from to = MV.swap v (from `mod` len) (to `mod` len)
  where len = MV.length v
