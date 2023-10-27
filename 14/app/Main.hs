module Main where

import Data.List (sort, tails)
import Data.List.Split

main :: IO ()
main = putStrLn "Hello, Haskell!"

type Point = (Int, Int)

makeLine :: String -> [Point]
makeLine xs =
    let samples = parseLine xs
        windows = slidingWindows 2 samples
     in concatMap interpolatePoints windows

parseLine :: String -> [Point]
parseLine = map parsePoint . filter (/= "->") . words

parsePoint :: String -> Point
parsePoint xs =
    let [x, y] = splitWhen (== ',') xs
     in (read x, read y)

slidingWindows :: Int -> [a] -> [[a]]
slidingWindows n = map (take n) . filter ((>= n) . length) . tails

interpolatePoints :: [Point] -> [Point]
interpolatePoints [(x1, y1), (x2, y2)]
    | x1 == x2 = [(x, y) | x <- [x1], y <- [minY .. maxY]]
    | y1 == y2 = [(x, y) | x <- [minX .. maxX], y <- [y1]]
    | otherwise = error "not a straight line"
  where
    minX = min x1 x2
    maxX = max x1 x2
    minY = min y1 y2
    maxY = max y1 y2
