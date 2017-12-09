module Day9 ( parseStream
            , countGroups
            , countGarbage
            , countAllGroups
            , filterGroups
            , Group(..)
            ) where

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, anyChar)
import Text.Parsec.Combinator (choice, manyTill, many1)

data Group = Group [Group] | Garbage String deriving (Eq, Show)

parseStream :: String -> [Group]
parseStream = either (error . show) id . parse parseGroup ""

countAllGroups :: [String] -> (Int, Int)
countAllGroups xs =
  let (groups, garbage) = unzip $ map (countGroups &&& countGarbage) xs in
    (sum groups, sum garbage)

countGarbage :: String -> Int
countGarbage = countGarbage' . parseStream

countGarbage' :: [Group] -> Int
countGarbage' [] = 0
countGarbage' (Garbage garbage:gs) = length garbage + countGarbage' gs
countGarbage' (Group g:gs) = countGarbage' g + countGarbage' gs

countGroups :: String -> Int
countGroups = countGroups' 1 .  parseStream

countGroups' :: Int -> [Group] -> Int
countGroups' _ [] = 0
countGroups' i (Garbage _:gs) = countGroups' i gs
countGroups' i (Group g:gs) =
  i + countGroups' (i+1) g + countGroups' i gs

filterGroups :: [Group] -> [Group]
filterGroups [] = []
filterGroups (Garbage _:gs) = filterGroups gs
filterGroups (Group g:gs) = (Group $ filterGroups g):filterGroups gs

parseGroup :: Parser [Group]
parseGroup = many1 (do
  char '{'
  groups <- manyTill (choice [ ignoreBang
                             , ignoreGarbage
                             , parseGroup
                             , char ',' >> return []
                             ]) (char '}')
  return $ Group (concat groups)
  )

ignoreBang :: Parser [Group]
ignoreBang = char '!' >> anyChar >> return []

ignoreGarbage :: Parser [Group]
ignoreGarbage = do
  char '<'
  garbage <- manyTill ((ignoreBang >> return [])
                        <|> ((:[]) <$> anyChar))
                      (char '>')
  return [Garbage (concat garbage)]
