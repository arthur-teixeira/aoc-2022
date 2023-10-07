module Main where

import Data.List
import Data.Maybe

main :: IO ()
main = interact $ (++"\n") . show . solve

solve = sum . map itemPriority . lines
  where
    itemPriority = getItemPriority . itemInBothCompartments . compartments

type Rucksack = (String, String)

compartments :: String -> Rucksack
compartments xs = splitAt (length xs `div` 2) xs

itemInBothCompartments :: Rucksack -> Char
itemInBothCompartments (x:xs, ys)
  | x `elem` ys = x
  | otherwise = itemInBothCompartments (xs, ys)

getItemPriority :: Char -> Int
getItemPriority x = (1 +) $ fromMaybe 0 (x `elemIndex` chars)
  where
    chars = ['a' .. 'z'] ++ ['A' .. 'Z']
