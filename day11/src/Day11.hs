module Day11 ( hexDistance
             , maxDistance
             ) where

data HexDirection = Northeast | North | Northwest
                  | Southwest | South | Southeast deriving (Eq, Show)

hexDistance :: [String] -> Int
hexDistance = distance . foldl moveHex (0, 0) . map toDirection

maxDistance :: [String] -> Int
maxDistance dirs = let moves = scanl moveHex (0, 0) . map toDirection $ dirs in
  maximum $ map distance moves

toDirection :: String -> HexDirection
toDirection s = case s of
  "n" -> North
  "ne" -> Northeast
  "nw" -> Northwest
  "s" -> South
  "sw" -> Southwest
  "se" -> Southeast
  _ -> error $ "invalid string " ++ s

moveHex :: (Int, Int) -> HexDirection -> (Int, Int)
moveHex (x, y) dir = case dir of
  North     -> (x  , y + 2)
  Northeast -> (x+2, y + 1)
  Northwest -> (x-2, y + 1)
  South     -> (x  , y - 2)
  Southeast -> (x+2, y - 1)
  Southwest -> (x-2, y - 1)

distance :: (Int, Int) -> Int
distance (x, y)
  | x == 0 && y == 0 = 0 -- origin
  | x == 0 && y /= 0 = abs y `div` 2 -- due north/south
  | x /= 0 && y == 0 = abs x `div` 2 -- due east/west
  | otherwise = -- move on a diagonal
    let moves = abs x `div` 2 in
      moves + distance (0, if y > 0 then y - moves else y + moves)
