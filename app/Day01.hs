module Day01 where

import Data.List (sortBy)
import Data.List.Split
import Data.Ord

main :: IO ()
main = interact (show . solve)

solve :: String -> Int
solve input =
  let convertElf = sum . map read
      splitInput = splitWhen (== "") . lines
      elves = map convertElf $ splitInput input
   in sum . take 3 . sortBy (comparing Data.Ord.Down) $ elves
