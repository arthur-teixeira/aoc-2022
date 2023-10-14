{-# LANGUAGE ParallelListComp #-}

module Main where

import Control.Applicative (ZipList(ZipList, getZipList))
import Data.Char (digitToInt)
import Data.List (transpose)

main :: IO ()
main = interact $ show . partOne

partOne = allVisible . visibility . parse

visibleX :: Int -> [Int] -> [Bool]
visibleX _ [] = []
visibleX maxL (x:xs)
  | x > maxL = True : visibleX x xs
  | otherwise = False : visibleX maxL xs

visibleLR :: [Int] -> [Bool]
visibleLR xs = getZipList $ (||) <$> left <*> right
  where
    left = ZipList $ visibleX (-1) xs
    right = ZipList $ reverse . visibleX (-1) . reverse $ xs

parse :: String -> [[Int]]
parse xs = map (map digitToInt) $ lines xs

visibility :: [[Int]] -> [[Bool]]
visibility xs = [getZipList $ (||) <$> a <*> b | a <- x | b <- y]
  where
    x = map (ZipList . visibleLR) xs
    y = map ZipList $ transpose $ map visibleLR $ transpose xs

allVisible :: [[Bool]] -> Int
allVisible [] = 0
allVisible (x:xs) = sumBools x + allVisible xs
    where
        sumBools = sum . map fromEnum
