module Day3 (memoryDistance
            ,drawGrid
            ,spiral
            ,Dir (..)
            ,fillSquares
            ,squareWithValue
            )
where

import qualified Data.Map.Lazy as Map
import Data.List (find)
import Data.Maybe (mapMaybe, fromJust)

data Dir = North | South | East | West deriving (Show, Eq)

nextDir :: Dir -> Dir
nextDir North = West
nextDir South = East
nextDir East  = North
nextDir West  = South

next :: (Int, Int) -> Dir -> (Int, Int)
next (x, y) dir = case dir of
                    North -> (x, y+1)
                    South -> (x, y-1)
                    East  -> (x+1, y)
                    West  -> (x-1, y)

data SpiralState = SpiralState { nextCorner     :: Int
                               , nextNextCorner :: Int
                               , distance       :: Int
                               , direction      :: Dir
                               }

nextState :: SpiralState -> SpiralState
nextState s
  | nc == i = SpiralState { nextCorner = nextNextCorner s,
                            nextNextCorner = if nextCorner s == nextNextCorner s
                                             then nextCorner s + 1
                                             else nextNextCorner s,
                            distance = 1,
                            direction = nextDir $ direction s
                          }
  | nc /= i = s { distance = i + 1}
  where nc = nextCorner s
        i = distance s

spiral :: [Dir]
spiral = spiral' (SpiralState 1 1 1 East)

spiral' :: SpiralState -> [Dir]
spiral' s = (direction s):(spiral' $ nextState s)

drawGrid :: [(Int, Int)]
drawGrid = scanl next (0,0) spiral

memoryDistance :: Int -> Int
memoryDistance i =
  let (x, y) = last $ take i drawGrid in
    abs x + abs y

-- Part B

fillSquares :: [Int]
fillSquares = fillSquares' (Map.singleton (0,0) 1) drawGrid

fillSquares' :: GridMap -> [(Int, Int)] -> [Int]
fillSquares' m (p:ps) =
  let val = sumNeighbours p m in
  val:(fillSquares' (Map.insert p val m) ps)

type GridMap = Map.Map (Int, Int) Int

sumNeighbours :: (Int, Int) -> GridMap -> Int
sumNeighbours (x, y) m =
  let lookups = [(x-1, y+1), (x, y+1), (x+1, y+1)
                ,(x-1, y  ), (x, y  ), (x+1, y  )
                ,(x-1, y-1), (x, y-1), (x+1, y-1)
                ] in
    sum $ mapMaybe (`Map.lookup`m) lookups

squareWithValue :: Int -> Int
squareWithValue x = fromJust $ find (>x) fillSquares
