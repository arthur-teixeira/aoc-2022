module Day10
  ( solveDay
  ) where

import Day (DaySolver)

solveDay :: DaySolver
solveDay input = do
  putStrLn $ partOne input
  putStrLn $ partTwo input

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
doInstruction prevCycles@(x:_) Noop = x : prevCycles
doInstruction prevCycles@(x:_) (Addx y) = (x + y) : x : prevCycles

getCycles :: [Instruction] -> [Int]
getCycles = reverse . foldl doInstruction [1]

getIndexes :: [Int] -> [Int] -> [Int]
getIndexes idxs xs = [x | (i, x) <- zip [1 ..] xs, i `elem` idxs]

partOne :: String -> String
partOne xs =
  let cycles = getCycles . parse $ xs
      cyclesToCheck = [20,60 .. 220] :: [Int]
      checked = getIndexes cyclesToCheck cycles
      mult = zipWith (*) cyclesToCheck checked
   in "Part one: " ++ (show . sum) mult

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs =
  let (chunk, rest) = splitAt n xs
   in chunk : chunks n rest

partTwo :: String -> String
partTwo xs =
  let cycles = init . getCycles . parse $ xs
      pixels = drawPixels cycles
   in "Part two: \n" ++ (unlines . chunks 40) pixels

spriteRange :: Int -> [Int]
spriteRange x = [x - 1, x, x + 1]

drawPixels :: [Int] -> [Char]
drawPixels = drawPixels' 0

drawPixels' :: Int -> [Int] -> [Char]
drawPixels' _ [] = []
drawPixels' curCycleIndex (curX:xs) =
  let curPixel = drawPixel curX
   in curPixel : drawPixels' (curCycleIndex + 1) xs
  where
    drawPixel x
      | (curCycleIndex `mod` 40) `elem` spriteRange x = '#'
      | otherwise = '.'
