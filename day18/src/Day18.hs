module Day18 ( parse
             , Instruction(..)
             , Val(..)
             , duetA
             , duetB
             ) where

import Control.Applicative ((<|>))
import Control.Monad.State.Lazy (State, gets, get, put, modify, execState)
import Control.Monad (when, unless)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust, fromJust)
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

duetA :: [String] -> Int
duetA instrs = let instrs' = map parse instrs in
  head . sendQueue $ execState duet' Duet { registers = Map.empty
                                   , pointer   = 0
                                   , program   = instrs'
                                   , programId = Nothing
                                   , coProgram = Nothing
                                   , sendCount = 0
                                   , sendQueue = []
                                   }

duetB :: [String] -> Int
duetB instrs =
  let instrs' = map parse instrs
      duet = Duet { registers = Map.empty
                  , pointer   = 0
                  , program   = instrs'
                  , programId = Just 0
                  , coProgram = Nothing
                  , sendCount = 0
                  , sendQueue = []
                  }
      coDuet = duet { coProgram = Just duet { programId = Just 1}} in
  getCount $ execState duet' coDuet

getCount :: Duet -> Int
getCount duet = fromJust $ do
  pid <- programId duet
  if pid == 1
    then return $ sendCount duet
    else sendCount <$> coProgram duet

data Duet = Duet { registers :: Map.Map Char Int
                 , pointer   :: Int
                 , program   :: [Instruction]
                 , programId :: Maybe Int
                 , coProgram :: Maybe Duet
                 , sendCount :: Int
                 , sendQueue :: [Int]
                 } deriving Show

duet' :: State Duet ()
duet' = do
  maybeInstr <- getInstruction
  case maybeInstr of
    Nothing -> do
      contextSwitch
      instr' <- getInstruction
      when (isJust instr') duet'
    (Just instr) ->
      case instr of
        r@Rcv{} -> handleReceive r
        s@Snd{} -> handleSend s
        Jgz x y -> do
          val0 <- gets $ getValue x
          if val0 > 0
            then gets (getValue y) >>= incrementInstr'
            else incrementInstr
          duet'
        Set x y -> gets (getValue y) >>= setRegister x >> incrementInstr >> duet'
        (Add x y) -> doOperator x y (+) >> duet'
        (Mod x y) -> doOperator x y mod >> duet'
        (Mul x y) -> doOperator x y (*) >> duet'

handleSend :: Instruction -> State Duet ()
handleSend (Snd val) = do
  duet <- get
  x <- gets $ getValue val
  count <- gets sendCount
  put duet { sendQueue = x:sendQueue duet
           , sendCount = count + 1 }
  incrementInstr
  duet'
handleSend _ = error "send only!"

handleReceive :: Instruction -> State Duet ()
handleReceive (Rcv val) = do
  duet <- get
  case coProgram duet of
    -- Part A
    Nothing -> do
      x <- gets $ getValue val
      when (x == 0) (incrementInstr >> duet')
    -- Part B
    Just coDuet ->
      case sendQueue coDuet of
        [] -> do
          contextSwitch
          instr' <- getInstruction
          case instr' of
            -- Deadlock if our sendQueue is empty
            Just (Rcv _) -> unless (null $ sendQueue duet) duet'
            _ -> duet'
        xs -> do
          let (x, xs') = (last xs, init xs)
          let (Register r) = val
          put duet { coProgram = Just coDuet { sendQueue = xs' } }
          setRegister r x
          incrementInstr
          duet'
handleReceive _ = error "send only!"

contextSwitch :: State Duet ()
contextSwitch = do
  duet <- get
  case coProgram duet of
    Just coDuet -> put $! coDuet { coProgram = Just duet { coProgram = Nothing } }
    Nothing -> return ()

doOperator :: Char -> Val -> (Int -> Int -> Int) -> State Duet ()
doOperator reg val f = do
  x <- gets $ getValue (Register reg)
  y <- gets $ getValue val
  let x' = f x y
  setRegister reg x'
  incrementInstr

setRegister :: Char -> Int -> State Duet ()
setRegister r x' = do
  m <- gets registers
  let m' = Map.insert r x' m
  modify $ \d -> d { registers = m'}

getInstruction :: State Duet (Maybe Instruction)
getInstruction = do
  i <- gets pointer
  instrs <- gets program
  if i < 0 || i > (length instrs - 1)
    then return Nothing
    else return $ Just (instrs !! i)

getValue :: Val -> Duet -> Int
getValue (Num i) _ = i
getValue (Register r) duet = Map.findWithDefault (if r == 'p' && isJust (programId duet)
                                                   then fromJust $ programId duet
                                                   else 0)
                               r $ registers duet

incrementInstr :: State Duet ()
incrementInstr = incrementInstr' 1

incrementInstr' :: Int -> State Duet ()
incrementInstr' jmp = do
  i <- gets pointer
  modify (\d -> d { pointer = i + jmp})

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
