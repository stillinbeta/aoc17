module Day7 ( parseLine
            , Program (..)
            , rootProgram
            , getKey
            , balanceNum
            , findOddOut
            ) where

import Control.Monad.Trans.State (execState, State, gets, modify)
import Control.Monad (mapM, mapM_, when)
import Data.Maybe (fromMaybe)
import Data.List (findIndex)
import qualified Data.Map.Strict as Map
import Text.Parsec (parse)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (letter, space, digit, char, string)
import Text.Parsec.Combinator (between, many1, between, optionMaybe, sepBy1)

data Program = Program { pWeight :: Int
                       , pName :: String
                       , pChildren :: [Program]
                       } | ProgramPointer String deriving (Eq, Show)


rootProgram :: [String] -> Program
rootProgram xs = let parsed = map parseLine xs
                     ltable = Map.fromList $ zip (map getKey parsed) parsed in
                   head . Map.elems $ execState reduce ltable

reduce :: State (Map.Map String Program) ()
reduce = do
  programs <- gets Map.elems
  mapM_ (collapseKey True) programs
  len <- gets Map.size
  when (len > 1) reduce


balanceNum :: Program -> Int
balanceNum p = case balanceNum' p of
                 Right _ -> error "tree is balance"
                 Left i -> i

balanceNum' :: Program -> Either Int Int
balanceNum' (ProgramPointer _) = error "Pointer in tree"
balanceNum' p@(Program weight _ children)  = do
  case children of
    [] -> Right weight
    _ -> do
      weights <- mapM balanceNum' children
      case findOddOut weights of
        Nothing -> Right $ sum weights + weight
        Just idx ->
          let program = children !! idx
              thisWeight = weights !! idx
              others = if idx == 0
                       then weights !! 1
                       else weights !! 0 in
            Left $ others - thisWeight + pWeight program


findOddOut :: [Int] -> Maybe Int
findOddOut (x1:x2:x3:xs)
  | x1 == x2 && x2 /= x3 = Just 2
  | x1 == x3 && x1 /= x2 = Just 1
  | x2 == x3 && x1 /= x2 = Just 0
  | otherwise = (3+) <$> findIndex (/=x1) xs
findOddOut _ = Nothing



collapseKey :: Bool -> Program -> State (Map.Map String Program) Program
collapseKey reInsert program = do
  let key = getKey program
  maybeProgram <- gets (Map.lookup key)
  case maybeProgram of
    -- Always remove pointers, they're never the final answer
    Just (p@ProgramPointer{})-> removeKey p
    Just p@(Program _ _ children) -> do
        -- collapse all children, removing them from root map
        children' <- mapM (collapseKey False) children
        let p' = p{ pChildren = children' }
        if reInsert
          -- We're at the root level, put the update back in
          then modify $ Map.insert key p'
          -- We're not at the root level, remove extraneous items
          else modify $ Map.delete key
        return p'
    Nothing -> return program

removeKey :: Program -> State (Map.Map String Program) Program
removeKey p@Program{} = return p
removeKey p@(ProgramPointer k) = do
  p' <- fromMaybe p <$> gets (Map.lookup k)
  modify $ Map.delete k
  return p'

getKey :: Program -> String
getKey (ProgramPointer s) = s
getKey p = pName p

parseLine :: String -> Program
parseLine s = case parse pProgram "" s of
  Left e -> error $ show e
  Right p -> p

pProgram :: Parser Program
pProgram = do
  name <- programName
  space
  num <- read <$> between (char '(') (char ')') (many1 digit)
  pointers <- map ProgramPointer . fromMaybe [] <$> (optionMaybe $ do
    string " -> "
    sepBy1 programName (string ", "))
  return $ Program num name pointers



programName :: Parser String
programName = many1 letter
