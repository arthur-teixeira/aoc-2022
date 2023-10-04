module Main where

import Data.List.Split
import Data.List

main :: IO ()
main = interact solve

solve :: String -> String
solve input =
  let elves = map convertElf $ splitInput input
   in show . sum $ take 3 $ (reverse . sort) elves
  where
    convertElf = sum . map read
    splitInput = splitWhen (== "") . lines
