module Day06
  ( solveDay
  ) where

import Data.List (tails)
import Day (DaySolver)

solveDay :: DaySolver
solveDay = print . firstUnique . substrs

substrs :: String -> [String]
substrs xs = filter ((== 14) . length) $ map (take 14) $ tails xs

firstUnique :: [String] -> Int
firstUnique xs = firstUnique' xs $ (length . head) xs

firstUnique' :: [String] -> Int -> Int
firstUnique' [] _ = 0
firstUnique' (x:xs) n
  | areUnique x = n
  | otherwise = firstUnique' xs (n + 1)

areUnique :: String -> Bool
areUnique [] = True
areUnique (x:xs)
  | x `elem` xs = False
  | otherwise = areUnique xs
