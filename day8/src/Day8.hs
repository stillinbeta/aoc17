module Day8 ( parseCmd
            , largestVariable
            , Cmd(..)
            , Comparison(..)
            ) where

import Control.Monad (when, mapM_)
import Control.Monad.Trans.State (State, gets, modify, execState, put)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Text.Parsec (parse, (<|>))
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator (many1, optionMaybe)
import Text.Parsec.Char (digit, letter, space, string, char, oneOf)

largestVariable :: [String] -> (Int, Int)
largestVariable strs =
  let cmds = map parseCmd strs
      emptyState = RegisterState { registers = Map.empty
                                 , maxVal = 0
                                 }
      finalRegs = execState (mapM_ runCmd cmds) emptyState in
    (maximum . Map.elems . registers $ finalRegs, maxVal finalRegs)

parseCmd :: String -> Cmd
parseCmd = either (error . show) id . parse pCmd ""

data Cmd = Cmd { variable :: String
               , delta :: Int
               , cmpVariable :: String
               , comparison :: Comparison
               , cmpNumber :: Int
               } deriving (Eq ,Show)

data Comparison = GreaterThan
                | LessThan
                | GreaterThanOrEqual
                | LessThanOrEqual
                | EqualTo
                | NotEqualTo deriving (Eq, Show)


data RState = RegisterState { registers :: Map.Map String Int
                            , maxVal :: Int
                            }

type RegisterState = State RState

updateVal :: String -> Int -> RegisterState ()
updateVal r v = do
  m <- gets registers
  oldMax <- gets maxVal
  let m' = Map.insert r v m
  put RegisterState { registers = m'
                    , maxVal = max oldMax v
                    }

runCmd :: Cmd -> RegisterState ()
runCmd cmd = do
  run <- predicateHolds cmd
  when run $ do
    let var = variable cmd
    val <- getValue var
    updateVal var (val + delta cmd)

predicateHolds :: Cmd -> RegisterState Bool
predicateHolds cmd = do
  val <- getValue (cmpVariable cmd)
  let cmp = case comparison cmd of
              GreaterThan -> (>)
              LessThan -> (<)
              GreaterThanOrEqual -> (>=)
              LessThanOrEqual -> (<=)
              EqualTo -> (==)
              NotEqualTo -> (/=)
  return $ val `cmp` cmpNumber cmd

getValue :: String -> RegisterState Int
getValue str = gets (Map.findWithDefault 0 str . registers)

-- Parsers --

pCmd :: Parser Cmd
pCmd = do
  var1 <- pVariable
  space
  d <- pDelta
  string " if "
  var2 <- pVariable
  space
  cmp <- pComparison
  space
  num <- pNum
  return $ Cmd { variable = var1
               , delta = d
               , cmpVariable = var2
               , comparison = cmp
               , cmpNumber = num
               }

pVariable :: Parser String
pVariable = many1 letter

pDelta :: Parser Int
pDelta = do
  opr <- string "dec" <|> string "inc"
  space
  num <- pNum
  return $ if opr == "dec"
           then num * (-1)
           else num

pNum :: Parser Int
pNum = do
  isNeg <- isJust <$> optionMaybe (char '-')
  num <- read <$> many1 digit
  return $ if isNeg
           then num * (-1)
           else num

pComparison :: Parser Comparison
pComparison = do
  str <- many1 $ oneOf "><=!"
  case str of
    ">"  -> return GreaterThan
    ">=" -> return GreaterThanOrEqual
    "<"  -> return LessThan
    "<=" -> return LessThanOrEqual
    "==" -> return EqualTo
    "!=" -> return NotEqualTo
    _ -> fail $ "unrecognised operator " ++ str
