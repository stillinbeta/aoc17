module Day18 ( parse
             , Instruction(..)
             , Val(..)
             , duet
             ) where

import Control.Applicative ((<|>), empty)
import Control.Monad.State.Lazy (StateT, gets, lift, modify, evalStateT)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1)
import Text.Parsec.Char (lower, char, digit, space)

data Instruction = Snd Val
                 | Set Char Val
                 | Add Char Val
                 | Mul Char Val
                 | Mod Char Val
                 | Rcv Val
                 | Jgz Val Val
                 deriving (Eq, Show)

data Val = Num Int | Register Char deriving (Eq, Show)

duet :: [String] -> Int
duet instrs = let instrs' = map parse instrs in
  fromJust $ evalStateT duet' Duet { registers = Map.empty
                                   , pointer = 0
                                   , lastSound = Nothing
                                   , program = instrs'
                                   }

data Duet = Duet { registers :: Map.Map Char Int
                 , pointer   :: Int
                 , lastSound :: Maybe Int
                 , program   :: [Instruction]
                 }

duet' :: StateT Duet Maybe Int
duet' = do
  instr <- getInstruction
  case instr of
    Rcv x -> do
      val <- getValue x
      if val == 0
        then incrementInstr >> duet'
        else gets lastSound >>= lift
    Snd x -> do
      val <- getValue x
      modify (\d -> d { lastSound = Just val})
      incrementInstr >> duet'
    Jgz x y -> do
      val0 <- getValue x
      if val0 > 0
        then getValue y >>= incrementInstr'
        else incrementInstr
      duet'
    Set x y -> getValue y >>= setRegister x >> incrementInstr >> duet'
    (Add x y) -> doOperator x y (+) >> duet'
    (Mod x y) -> doOperator x y mod >> duet'
    (Mul x y) -> doOperator x y (*) >> duet'


doOperator :: Char -> Val -> (Int -> Int -> Int) -> StateT Duet Maybe ()
doOperator reg val f = do
  x <- getValue (Register reg)
  y <- getValue val
  let x' = f x y
  setRegister reg x'
  incrementInstr

setRegister :: Char -> Int -> StateT Duet Maybe ()
setRegister r x' = do
  m <- gets registers
  let m' = Map.insert r x' m
  modify $ \d -> d { registers = m'}



getInstruction :: StateT Duet Maybe Instruction
getInstruction = do
  i <- gets pointer
  instrs <- gets program
  if i < 0 || i > (length instrs - 1)
    then empty
    else return $ instrs !! i

getValue :: Val -> StateT Duet Maybe Int
getValue (Num i) = return i
getValue (Register r) = gets $ (Map.findWithDefault 0 r) . registers

incrementInstr :: StateT Duet Maybe ()
incrementInstr = incrementInstr' 1

incrementInstr' :: Int -> StateT Duet Maybe ()
incrementInstr' jmp = do
  i <- gets pointer
  modify (\d -> d { pointer = (i + jmp)})

parse :: String -> Instruction
parse = either (error . show) id . Text.Parsec.parse instruction ""

instruction :: Parser Instruction
instruction = do
  cmd <- many1 lower
  space
  case cmd of
    "snd" -> Snd <$> value
    "set" -> xy Set
    "add" -> xy Add
    "mul" -> xy Mul
    "mod" -> xy Mod
    "rcv" -> Rcv <$> value
    "jgz" -> do
      x <- value
      space
      y <- value
      return $ Jgz x y
    _ -> fail $ "unknown cmd " ++ cmd



xy :: (Char -> Val -> Instruction ) -> Parser Instruction
xy c = do
  x <- lower
  space
  y <- value
  return $ c x y


value :: Parser Val
value = Register <$> lower
      <|> (Num . read) <$> (many1 ((char '-') <|> digit))
