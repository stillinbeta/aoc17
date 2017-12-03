module Day3 (memoryDistance
            ,drawGrid
            ,spiral
            ,Dir (..)
            )
where

import Control.Monad (replicateM)
import Control.Monad.Trans.State

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

spiral :: Int -> [Dir]
spiral i = evalState (replicateM i spiral') (SpiralState 1 1 1 East)

spiral' :: State SpiralState Dir
spiral' = do
  dir <- gets direction
  modify nextState
  return dir

drawGrid :: Int -> [(Int, Int)]
drawGrid x = scanl next (0,0) (spiral $ x - 1)


memoryDistance :: Int -> Int
memoryDistance i =
  let (x, y) = last $ drawGrid i in
    abs x + abs y
