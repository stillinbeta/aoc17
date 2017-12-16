module Day16 ( Instruction(..)
             , makeVector
             , doInstruction
             , parse
             , dance
             , dance'
             , danceForever
             ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

makeVector :: Int -> ProgramList
makeVector = V.fromList . flip take ['a'..'z']

type ProgramList = V.Vector Char

data Instruction = Spin Int | Exchange Int Int | Partner Char Char deriving (Eq, Show)

dance :: [String] -> String
dance = dance' 16

dance' :: Int -> [String] -> String
dance' size instrs = let v = makeVector size in
  V.toList $ dance'' (map parse instrs) v

dance'' :: [Instruction] -> ProgramList -> ProgramList
dance'' instrs v = foldl doInstruction v instrs

danceForever :: [String] -> String
danceForever instrs = let v = makeVector 16
                          instrs' = map parse instrs in
  V.toList $ doN 1000000000 (dance'' instrs') v

doN :: Int -> (a -> a) -> a -> a
doN 0 _ a = a
doN i f a = doN (i-1) f (f a)

doInstruction :: ProgramList -> Instruction -> ProgramList
doInstruction v (Spin n) =
  let (v1, v2) = V.splitAt (V.length v - n) v in
    v2 V.++ v1
doInstruction v (Exchange i0 i1) = V.modify (\v' -> MV.swap v' i0 i1) v
doInstruction v (Partner c0 c1) =
  let i0 = fromJust $ V.elemIndex c0 v
      i1 = fromJust $ V.elemIndex c1 v in
    doInstruction v (Exchange i0 i1)

parse :: String -> Instruction
parse (c:cs) =
  case c of
    's' -> Spin $ read cs
    'x' -> let (i0, i1) = parse' cs in Exchange (read i0) (read i1)
    'p' -> let (c0, c1) = parse' cs in Partner (head c0) (head c1)
    _ -> error $ "unknown instruction" ++ [c]
parse _ = error "no parse"

parse' :: String -> (String, String)
parse' s = let idx = fromJust $ elemIndex '/' s in
             (take idx s, drop (idx+1) s)
