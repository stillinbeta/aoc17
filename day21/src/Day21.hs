module Day21 ( rotate
             , reflect
             , enhance
             , parse
             ) where

import qualified Data.Map.Strict as Map
import Data.List (transpose)
import Data.List.Split (splitOn)

import Debug.Trace

type PixelGrid = [[Bool]]

enhance :: Int -> [String] -> Int
enhance i strs = let m = foldr ((Map.union . insertAll) . parse) Map.empty strs in
  countOn $ enhance' i m grid0

countOn :: PixelGrid -> Int
countOn = sum . map (\x -> if x then 1 else 0) . concat


enhance' :: Int -> Map.Map PixelGrid PixelGrid -> PixelGrid -> PixelGrid
enhance' 0 _ g = g
enhance' i m g = let grids = splitIntoGroups g
                     gridList = map (m Map.!) grids
                     width = sqrt' $! length gridList in
  enhance' (i-1) m $! traceShowId (stitchTogether width gridList)

stitchTogether :: Int -> [PixelGrid] -> PixelGrid
stitchTogether _ [] = []
stitchTogether i xs = let (x, xs') = splitAt i xs in
  transpose (concat x) ++ stitchTogether i xs'

sqrt' :: Int -> Int
sqrt' = round . sqrt . fromIntegral

splitIntoGroups :: PixelGrid -> [PixelGrid]
splitIntoGroups g = let by = if length g `mod` 2 == 0 then 2 else 3 in
                        splitIntoGroups' by g

splitIntoGroups' :: Int -> PixelGrid -> [PixelGrid]
splitIntoGroups' _ [] = []
splitIntoGroups' i g =
  let (x, xs) = splitAt i g in
    chopUp i x
    ++ splitIntoGroups' 0 xs

chopUp :: Int -> PixelGrid -> [PixelGrid]
chopUp _ [] = []
chopUp i xs = let xs' = map (splitAt i) xs -- [([a, a])]
                  x = foldr ((:). fst) [] xs'
                  xs'' = foldr ((:). snd) [] xs'
                  in x:chopUp i xs''

parse :: String -> (PixelGrid, PixelGrid)
parse str = let [from, to] = splitOn " => " str in
  (f from, f to)
  where f = map (map (=='#')) . splitOn "/"

grid0 :: PixelGrid
grid0 = [[ False,  True, False ]
        ,[ False, False,  True ]
        ,[  True,  True,  True ]]

insertAll :: (PixelGrid, PixelGrid) -> Map.Map PixelGrid PixelGrid
insertAll pair = Map.fromList $ f pair ++ f (both reflect pair)
  where f = take 4 . iterate (both rotate)

rotate :: PixelGrid -> PixelGrid
rotate = reflect . transpose

reflect :: PixelGrid -> PixelGrid
reflect = map reverse

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, b) = (f a, f b)
