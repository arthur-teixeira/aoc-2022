module Main where

main :: IO ()
main = do
  contents <- getContents
  print $ solve contents

data Instruction
  = Noop
  | Addx Int
  deriving (Show)

parseInstruction :: String -> Instruction
parseInstruction xs =
  case words xs of
    ["addx", x] -> Addx $ read x
    ["noop"] -> Noop

parse :: String -> [Instruction]
parse = map parseInstruction . lines

doInstruction :: [Int] -> Instruction -> [Int]
doInstruction prevCycles@(x:xs) Noop = x : prevCycles
doInstruction prevCycles@(x:xs) (Addx y) = (x + y) : x : prevCycles

getCycles :: [Instruction] -> [Int]
getCycles = reverse . foldl doInstruction [1]

getIndexes :: [Int] -> [Int] -> [Int]
getIndexes idxs xs = [x | (i, x) <- zip [1 ..] xs, i `elem` idxs]

solve :: String -> Int
solve xs =
  let cycles = getCycles . parse $ xs
      cyclesToCheck = [20,60 .. 220] :: [Int]
      checked = getIndexes cyclesToCheck cycles
      mult = zipWith (*) cyclesToCheck checked
   in sum mult
