module Day19 ( followLabyrinth
             ) where

import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust, isNothing)
import Data.Char (isAlpha)
import Data.List (find)
import qualified Data.Vector as V

type VLab = V.Vector (V.Vector Char)

dirMap :: Map.Map Direction ((Int, Int) -> (Int, Int))
dirMap = Map.fromList [ (North, \(x, y) -> (x, y-1))
                      , (South, \(x, y) -> (x, y+1))
                      , (East,  \(x, y) -> (x+1, y))
                      , (West,  \(x, y) -> (x-1, y))
                      ]

reflect :: Direction -> Direction
reflect dir = case dir of
  North -> South
  South -> North
  East  -> West
  West  -> East

followDir :: (Int, Int) -> Direction -> (Int, Int)
followDir xy dir= dirMap Map.! dir $ xy

data Direction = North | South | East | West deriving (Eq, Ord, Show)

followLabyrinth :: [String] -> (Int, String)
followLabyrinth lab = let vlab = V.fromList $ map V.fromList lab
                          x = fromJust $ V.elemIndex '|' (vlab V.! 0) in
  followLabyrinth' 0 (x, 0) South vlab



followLabyrinth' :: Int -> (Int, Int) -> Direction -> VLab -> (Int, String)
followLabyrinth' i xy dir vlab
  -- a junction
  | chr ?= '+' = let dirMap' = Map.delete (reflect dir) dirMap -- reflect so we don't backtrack
                     -- Get a list of all characters
                     dirs = Map.toList $ Map.map (\fxy ->
                                                 getValue (fxy xy) vlab) dirMap'
                     (dir', _) = fromJust $
                       find (\(_, chr') -> maybe False (/=' ') chr') dirs in
      followLabyrinth' i' (followDir xy dir') dir' vlab
  -- we've reached the end of the map
  | isNothing chr = (i, "")
  -- a space means we're at the end
  | chr ?= ' ' = (i, "")
  -- Found a letter! keep going
  | maybe False isAlpha chr =  (fromJust chr:) <$> followLabyrinth' i' (followDir xy dir) dir vlab
  -- a - or a |
  | otherwise = followLabyrinth' i' (followDir xy dir) dir vlab
  where chr = getValue xy vlab
        i' = i + 1

getValue :: (Int, Int) -> VLab -> Maybe Char
getValue (x, y) vlab = (vlab V.!? y) >>= (V.!? x)

(?=) :: Eq a => Maybe a -> a -> Bool
Nothing ?= _ = False
(Just a) ?= b = a == b
