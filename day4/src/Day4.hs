module Day4 (isValid
            ,isValidAnagrams
            ,countValid
            ,countValidAnagarms
            ) where

import Data.List (permutations)
import qualified Data.Set as Set

isValid :: String -> Bool
isValid = isValid' Set.empty . words

isValid' :: Set.Set String -> [String] -> Bool
isValid' _ [] = True
isValid' s (x:xs) =
  not (x `Set.member` s) && isValid' (x `Set.insert` s) xs


countValid :: [String] -> Int
countValid = length . filter id . map isValid

-- Part b
isValidAnagrams :: String -> Bool
isValidAnagrams = isValidAnagrams' Set.empty . words

isValidAnagrams' :: Set.Set String -> [String] -> Bool
isValidAnagrams' _ [] = True
isValidAnagrams' s (x:xs) =
  let perms = permutations x in
  all (not . (`Set.member`s)) perms && isValidAnagrams' (foldr Set.insert s perms) xs


countValidAnagarms :: [String] -> Int
countValidAnagarms = length . filter id . map isValidAnagrams
