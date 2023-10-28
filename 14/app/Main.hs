module Main where

import Data.List (nub, sort, tails)
import Data.List.Split (splitWhen)

import Control.Monad.State

main :: IO ()
main = do
    contents <- getContents
    putStrLn $ "Part one: " <> show (partOne contents)

partOne xs = evalState (doGame . makeLines $ xs) 0

type Point = (Int, Int)

makeLines :: String -> [Point]
makeLines = concatMap makeLine . lines

makeLine :: String -> [Point]
makeLine xs =
  let samples = parseLine xs
      windows = slidingWindows 2 samples
   in nub $ concatMap interpolatePoints windows

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
  | x1 == x2 =
    if y2 > y1
      then [(x, y) | x <- [x1], y <- [y1 .. y2]]
      else reverse [(x, y) | x <- [x1], y <- [y2 .. y1]]
  | y1 == y2 =
    if x2 > x1
      then [(x, y) | x <- [x1 .. x2], y <- [y1]]
      else reverse [(x, y) | x <- [x2 .. x1], y <- [y1]]

below :: Point -> Point
below (x, y) = (x, y + 1)

diagLeft :: Point -> Point
diagLeft (x, y) = (x - 1, y + 1)

diagRight :: Point -> Point
diagRight (x, y) = (x + 1, y + 1)

dropSand = dropSand' start
  where
    start = (500, 0)

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes x f = foldr1 (.) (replicate x f)

hasBelow :: Point -> [Point] -> Bool
hasBelow (x, y) = any ((== x) . fst)

dropSand' :: Point -> [Point] -> [Point]
dropSand' sand points
  | not $ hasBelow sand points = points
  | below sand `notElem` points = dropSand' (below sand) points
  | diagLeft sand `notElem` points = dropSand' (diagLeft sand) points
  | diagRight sand `notElem` points = dropSand' (diagRight sand) points
  | otherwise = sand : points

type SandState = State Int Int

doGame :: [Point] -> SandState
doGame stones = do
  sandCount <- get
  let newState = dropSand stones
   in if newState == stones
        then return sandCount
        else do
          put (sandCount + 1)
          doGame newState
