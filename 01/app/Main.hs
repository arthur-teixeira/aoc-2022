module Main where

import Data.List.Split

main :: IO ()
main = interact solve

solve :: String -> String
solve input =
  let elves = map convertElf $ splitInput input
   in show $ maximum elves
  where
    convertElf = sum . map read
    splitInput = splitWhen (== "") . lines
