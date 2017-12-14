module Day12 ( countLinks
             , Link(..)
             , parseLink
             , parseCountLinks
             ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Text.Parsec (string, digit, sepBy1, many1, parse)
import Text.Parsec.String (Parser)

data Link = Link Int [Int] deriving (Eq, Show)
type LinkMap = Map.Map Int (Set.Set Int)

parseCountLinks :: [String] -> (Int, Int)
parseCountLinks strs = let m = makeMap $ map parseLink strs
                           seen = countLinks 0 m
                           partA = Set.size seen
                           notSeen = Set.fromList (Map.keys m) `Set.difference` seen
                           partB = 1 + countGroups seen notSeen m
                           in (partA, partB)

countGroups :: Set.Set Int -> Set.Set Int -> LinkMap -> Int
countGroups seen notSeen m
  | Set.null notSeen = 0
  | otherwise = let i = Set.elemAt 0 notSeen
                    visited = countLinks i m
                    seen' = seen `Set.union` visited
                    notSeen' = notSeen `Set.difference` visited in
                    1 + countGroups seen' notSeen' m

makeMap :: [Link] -> LinkMap
makeMap = foldl addLink Map.empty

countLinks :: Int -> LinkMap -> Set.Set Int
countLinks i m = countLinks' m Set.empty i

countLinks' :: LinkMap -> Set.Set Int -> Int -> Set.Set Int
countLinks' m s i
  | Set.member i s = s
  | otherwise = case Map.lookup i m of
      Nothing -> error $ "couldn't find " ++ show i
      Just dests -> Set.foldl (countLinks' m) (Set.insert i s) dests


addLink :: LinkMap -> Link -> LinkMap
addLink m (Link from to) = foldl (addToBoth from) m to

addToBoth :: Int -> LinkMap -> Int -> LinkMap
addToBoth f m t = addToDest (addToDest m f t) t f

addToDest :: LinkMap -> Int -> Int -> LinkMap
addToDest m f t =
  let s = Map.findWithDefault Set.empty f m in
  Map.insert f (Set.insert t s) m

parseLink :: String -> Link
parseLink = either (error . show) id . parse link ""

link :: Parser Link
link = do
  from <- read <$> many1 digit
  string " <-> "
  to <- map read <$> sepBy1 (many1 digit) (string ", ")
  return $ Link from to
