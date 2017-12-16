{-# LANGUAGE FlexibleContexts #-}

module Day14 ( squareUsed
             , usedSquares
             , regions
             ) where

import qualified Day10

import Numeric
import Data.Bits ((.&.), shift)

import Control.Monad (when)
import Control.Monad.ST (runST)
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

hexToInt :: String -> Integer
hexToInt str = case readHex str of
  [(num, "")] -> num
  _ -> error $ "no parse " ++ str

squareUsed :: String -> Int -> Int -> Bool
squareUsed salt row col = let hashInput = salt ++ "-" ++ show row
                              knotHash = hexToInt $ Day10.knotHash hashInput in
                            squareUsed' knotHash col == 1

squareUsed' :: Integer -> Int -> Int
squareUsed' knotHash col = if knotHash .&. shift 1 (127 - col) /= 0
                           then 1
                           else 0

rowUsed :: String -> Int -> V.Vector Int
rowUsed input row = let hashInput = input ++ "-" ++ show row
                        knotHash = hexToInt $ Day10.knotHash hashInput in
                      V.fromList [squareUsed' knotHash col | col <- [0..127]]

makeVector :: String -> V.Vector (V.Vector Int)
makeVector input = V.fromList [rowUsed input row | row <- [0..127] ]

usedSquares :: String -> Int
usedSquares input = let v = makeVector input in
                      foldr ((+) . sum) 0 v

-- Part B
type MMatrix m = MV.MVector (PrimState m) (MV.MVector (PrimState m) Int)

regions :: String -> Int
regions input = runST $ do
  let v = makeVector input
  v' <- MV.new $ V.length v
  mapM_ (\x -> V.thaw (v V.! x) >>= MV.write v' x) [0..V.length v - 1]
  arr <- mapM (regions' v') [(x, y) | x <- [0..127], y <- [0..127]]
  return $ sum arr

  -- let matrix = M.fromLists $ makeVector input
  -- mmatrix <- M.thaw matrix
  -- var <- mapM (regions' mmatrix) 0
  -- return 0

--regions' :: (PrimMonad s) => (M.Mutable m (MV.Mutable v) (PrimState s) Int)-> (Int, Int) -> s Int
regions' :: PrimMonad m => MMatrix m -> (Int, Int) -> m Int
regions' mat (x, y) = do
  v <- getValue mat (x, y)
  if v == 1
    then fillRegions mat (x, y) >> return 1
    else return 0

getValue :: PrimMonad m => MMatrix m -> (Int, Int) -> m Int
getValue mat (x, y) = do
  r <- MV.read mat x
  MV.read r y

writeValue :: PrimMonad m => MMatrix m -> (Int, Int) -> Int -> m ()
writeValue mat (x, y) val = do
  r <- MV.read mat x
  MV.write r y val

validCoordinate :: PrimMonad m => MMatrix m -> (Int, Int) -> m Bool
validCoordinate mat (x, y) =
  let rowLen = MV.length mat in
    if rowLen > x && x >= 0
    then do
      r <- MV.read mat x
      let colLen = MV.length r
      return $ colLen > y && y >= 0
    else return False

fillRegions :: PrimMonad m => MMatrix m -> (Int, Int) -> m ()
fillRegions mat xy@(x, y) = do
  valid <- validCoordinate mat xy
  when valid $ do
    v <- getValue mat xy
    when (v == 1) $ do
      writeValue mat xy 0
      mapM_ (fillRegions mat) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
