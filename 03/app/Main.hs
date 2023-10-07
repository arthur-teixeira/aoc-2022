module Main where

import Data.List

main :: IO ()
main = putStr "a"

solve = map (getItemPriority . itemInBothCompartments . compartments) . lines

type Rucksack = (String, String)

compartments :: String -> Rucksack
compartments xs = splitAt (length xs `div` 2) xs

itemInBothCompartments :: Rucksack -> Char
itemInBothCompartments (x:xs, ys)
    | x `elem` ys = x
    | otherwise = itemInBothCompartments (xs, ys)

getItemPriority :: Char -> Maybe Int
getItemPriority x
    | x `elem` lowers = (1 +) <$> x `elemIndex` lowers
    | otherwise = (27 +) <$> x `elemIndex` uppers
  where
    lowers = ['a' .. 'z']
    uppers = ['A' .. 'Z']
