module Day01 (solveDay) where

import Day (DaySolver)

import Data.List (sortBy)
import Data.List.Split
import Data.Ord

solveDay :: DaySolver
solveDay = print . solve

solve :: String -> Int
solve input =
  let convertElf = sum . map read
      splitInput = splitWhen (== "") . lines
      elves = map convertElf $ splitInput input
   in sum . take 3 . sortBy (comparing Data.Ord.Down) $ elves
