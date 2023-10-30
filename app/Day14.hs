module Day14
  ( solveDay
  ) where

import Data.List (nub, tails)
import Data.List.Split (splitWhen)
import qualified Data.Set as S

import Control.Monad.State
import Day (DaySolver)

solveDay :: DaySolver
solveDay input = do
  putStrLn $ "Part one: " <> show (partOne input)
  putStrLn $ "Part two: " <> show (partTwo input)

partOne :: [Char] -> Int
partOne xs = evalState (doPartOne rocks) 0
  where
    rocks = S.fromList $ makeLines xs

partTwo :: [Char] -> Int
partTwo xs = evalState (doPartTwo lowest rocks) 1
  where
    rocks = S.fromList $ makeLines xs
    lowest = findLowestRock rocks

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

dropSand :: S.Set Point -> S.Set Point
dropSand = dropSand' start
  where
    start = (500, 0)

dropSandPartTwo :: Int -> S.Set Point -> S.Set Point
dropSandPartTwo lowest = dropSandPartTwo' lowest start
  where
    start = (500, 0)

hasBelow :: Point -> S.Set Point -> Bool
hasBelow (x, _) = any ((== x) . fst)

dropSand' :: Point -> S.Set Point -> S.Set Point
dropSand' sand points
  | not $ hasBelow sand points = points
  | below sand `notElem` points = dropSand' (below sand) points
  | diagLeft sand `notElem` points = dropSand' (diagLeft sand) points
  | diagRight sand `notElem` points = dropSand' (diagRight sand) points
  | otherwise = S.insert sand points

findLowestRock :: S.Set Point -> Int
findLowestRock points = maximum xs
  where
    xs = map snd (S.toList points)

dropSandPartTwo' :: Int -> Point -> S.Set Point -> S.Set Point
dropSandPartTwo' lowest sand points
  | sand `elem` points = points
  | snd sand == lowest + 1 = S.insert sand points
  | below sand `notElem` points = dropSandPartTwo' lowest (below sand) points
  | diagLeft sand `notElem` points =
    dropSandPartTwo' lowest (diagLeft sand) points
  | diagRight sand `notElem` points =
    dropSandPartTwo' lowest (diagRight sand) points
  | otherwise = S.insert sand points

type SandState = State Int Int

doPartOne :: S.Set Point -> SandState
doPartOne stones = do
  sandCount <- get
  let newState = dropSand stones
   in if newState == stones
        then return sandCount
        else do
          put (sandCount + 1)
          doPartOne newState

doPartTwo :: Int -> S.Set Point -> SandState
doPartTwo lowest stones = do
  sandCount <- get
  let newState = dropSandPartTwo lowest stones
   in if (500, 0) `elem` newState
        then return sandCount
        else do
          put (sandCount + 1)
          doPartTwo lowest newState

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes x f = foldr1 (.) (replicate x f)
