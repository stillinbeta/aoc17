module Day20 ( parseParticle
             , closestParticle
             , survivingParticles
             , Vector(..)
             , Particle(..)
             , tick
             ) where

import Data.Maybe (fromMaybe)
import Data.Function (on)
import Data.List (minimumBy, groupBy)
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string, char, digit)
import Text.Parsec.Combinator (optionMaybe, many1)

data Vector = Vector Int Int Int deriving (Eq, Show)
data Particle = Particle { position :: Vector
                         , velocity :: Vector
                         , acceleration :: Vector
                         } deriving (Eq, Show)

-- part a

closestParticle :: [String] -> Int
closestParticle strs = let particles = zip [0..] $ map parseParticle strs in
  fst $ minimumBy (compare `on` (vectorMagnitude . acceleration . snd)) particles

vectorMagnitude :: Vector -> Double
vectorMagnitude (Vector x y z) =
  let (x', y', z') = (fromIntegral x, fromIntegral y, fromIntegral z) in
    sqrt ((x' ^^ 2) + (y' ^^ 2) + (z' ^^ 2))

-- part B

survivingParticles :: [String] -> Int
survivingParticles = length . survivingParticles' . map parseParticle

survivingParticles' :: [Particle] -> [Particle]
survivingParticles' particles = let particles' = loop1000 particles in
  if length particles == length particles'
  then particles'
  else survivingParticles' particles'

tick :: Particle -> Particle
tick (Particle   (Vector px py pz)
                 (Vector vx vy vz)
               a@(Vector ax ay az)) =
  let v'@(Vector vx' vy' vz') = Vector (vx + ax) (vy + ay) (vz+az)
      p' = Vector (px + vx') (py + vy') (pz + vz') in
    Particle p' v' a

loop1000 :: [Particle] -> [Particle]
loop1000 particles = foldr (const $ dedup . map tick) particles [0..999]

dedup :: [Particle] -> [Particle]
dedup = concat . filter ((==1) . length ) . groupBy ((==) `on` position)

-- Parser

parseParticle :: String -> Particle
parseParticle = either (error . show) id . parse pParticle ""

pParticle :: Parser Particle
pParticle = do
  string "p="
  p <- pVector
  string ", v="
  v <- pVector
  string ", a="
  a <- pVector
  return $ Particle p v a

pVector :: Parser Vector
pVector = do
  char '<'
  x <- pDigit
  char ','
  y <- pDigit
  char ','
  z <- pDigit
  char '>'
  return $ Vector x y z

pDigit :: Parser Int
pDigit = do
  negative <- fromMaybe ' ' <$> optionMaybe (char '-')
  num <- many1 digit
  return $ read (negative:num)
