module Day5 ( jumpsToEscapeA
            , jumpsToEscapeB
            , nextInstructionPartA
            , nextInstructionPartB
            , InstructionList (..)
            ) where

data InstructionList = InstructionList { iBack :: [Int]
                                       , iForward :: [Int]
                                       , instruction :: Int
                                       } deriving (Eq, Show)

nextInstruction :: (Int -> Int) -> InstructionList -> Maybe InstructionList
nextInstruction incr il
  | inst > 0  = do
      (b, f) <- maybeSplit (iForward il) inst
      let b' = reverse b
      return $ InstructionList (tail b' ++ inst':iBack il) f (head b')
  | inst < 0  = do
      (b, f) <- maybeSplit (iBack il) $ abs inst
      let b' = reverse b
      return $ InstructionList f (tail b' ++ inst':iForward il) (head b')
  | otherwise = return $ il { instruction = inst' }
  where inst = instruction il
        inst' = incr inst

nextInstructionPartA :: InstructionList -> Maybe InstructionList
nextInstructionPartA = nextInstruction (+1)

nextInstructionPartB :: InstructionList -> Maybe InstructionList
nextInstructionPartB = nextInstruction $ \x -> if x >= 3 then x - 1 else x + 1

maybeSplit :: [a] -> Int -> Maybe ([a], [a])
maybeSplit xs i =
  let (x1, x2) = splitAt i xs in
    if length x1 == i
    then Just (x1, x2)
    else Nothing

jumpsToEscapeA :: [Int] -> Int
jumpsToEscapeA [] = 0
jumpsToEscapeA (x:xs)= jumpsToEscape' nextInstructionPartA $ InstructionList [] xs x

jumpsToEscapeB :: [Int] -> Int
jumpsToEscapeB [] = 0
jumpsToEscapeB (x:xs)= jumpsToEscape' nextInstructionPartB $ InstructionList [] xs x


jumpsToEscape' :: (InstructionList -> Maybe InstructionList) ->  InstructionList -> Int
jumpsToEscape' ni i = let mi = ni i  in
                     case mi of
                       (Just i') -> 1 + jumpsToEscape' ni i'
                       Nothing -> 1
