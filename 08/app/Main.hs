{-# LANGUAGE ParallelListComp #-}

module Main where

import Control.Applicative (ZipList(ZipList, getZipList))
import Data.Char (digitToInt)
import Data.List (transpose)

main :: IO ()
main = do
    contents <- getContents
    putStrLn $ partOne contents
    putStrLn $ partTwo contents

partOne xs = "Part one: " ++ (show . allVisible . visibility . parse) xs
partTwo xs = "Part two: " ++ (show . maximum . concat . scenicScores . parse) xs

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

numVisibleTreesX :: [Int] -> [Int]
numVisibleTreesX [] = []
numVisibleTreesX [_] = [0]
numVisibleTreesX (x:xs) =
    let (visible, blocker) = span (< x) xs
        current = length visible + if null blocker then 0 else 1
        in current : numVisibleTreesX xs

numVisibleTreesLR :: [Int] -> [Int]
numVisibleTreesLR xs = getZipList $ (*) <$> left <*> right
  where
    left = ZipList $ numVisibleTreesX xs
    right = ZipList $ reverse . numVisibleTreesX . reverse $ xs

scenicScores :: [[Int]] -> [[Int]]
scenicScores xs = [getZipList $ (*) <$> a <*> b | a <- x | b <- y]
  where
    x = map (ZipList . numVisibleTreesLR) xs
    y = map ZipList $ transpose $ map numVisibleTreesLR $ transpose xs
