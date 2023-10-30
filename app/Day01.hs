module Day01 where

import Data.List
import Data.List.Split

main :: IO ()
main = interact (show . solve)

solve :: String -> Int
solve input =
  let convertElf = sum . map read
      splitInput = splitWhen (== "") . lines
      elves = map convertElf $ splitInput input
   in sum . take 3 . reverse . sort $ elves
