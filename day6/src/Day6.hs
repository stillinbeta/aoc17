module Day6 ( redistribute
            , cyclesTilLoop
            , findMaxIdx
            , toIMap
            ) where

import qualified Data.IntMap as IMap
import qualified Data.Map as Map

cyclesTilLoop :: [Int] -> (Int, Int)
cyclesTilLoop xs = cyclesTilLoop' 0 Map.empty $ toIMap xs

cyclesTilLoop' :: Int -> Map.Map (IMap.IntMap Int) Int -> IMap.IntMap Int -> (Int, Int)
cyclesTilLoop' i s m = let m' = redistribute m
                           i' = i + 1 in
                         case Map.lookup m' s of
                           Just li -> (i', i' - li)
                           Nothing -> cyclesTilLoop' i' (Map.insert m' i' s) m'

toIMap :: [Int] -> IMap.IntMap Int
toIMap = IMap.fromList . zip [0..]

redistribute :: IMap.IntMap Int -> IMap.IntMap Int
redistribute m =
  let (idx, val) = findMaxIdx m
      m' = IMap.insert idx 0 m in
      foldl incr m' (take val [idx+1..])

incr :: IMap.IntMap Int -> Int -> IMap.IntMap Int
incr m i = IMap.adjust (+1) (i `mod` IMap.size m) m

findMaxIdx :: IMap.IntMap Int -> (Int, Int)
findMaxIdx = foldl1 (\p0 p1 -> if snd p0 < snd p1 then p1 else p0) . IMap.toList
