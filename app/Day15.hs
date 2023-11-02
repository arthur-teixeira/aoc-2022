{-# LANGUAGE ViewPatterns, TupleSections #-}

module Day15
  ( solveDay
  ) where

import Data.List (nub, sort, stripPrefix)
import Data.List.Split (splitWhen)
import Day (DaySolver)

solveDay :: DaySolver
solveDay input = do
  putStrLn $ "Part One: " <> show (partOne input)
  putStrLn $ "Part Two: " <> show (partTwo input)

type Point = (Int, Int)

type Sensor = (Int, Int, Int)

type Beacon = (Int, Int)

data Cave = Cave
  { sensors :: [Sensor]
  , beacons :: [Beacon]
  } deriving (Show)

parseXY :: String -> (Int, Int)
parseXY xs =
  let ['x':'=':x, 'y':'=':y] = words . filter (/= ',') $ xs
   in (read x, read y)

parseBeacon :: String -> Beacon
parseBeacon (stripPrefix " closest beacon is at " -> Just point) = parseXY point

parseSensor :: String -> (Sensor, Beacon)
parseSensor (stripPrefix "Sensor at " -> Just input) =
  let [point, rest] = splitWhen (== ':') input
      beacon = parseBeacon rest
      (x, y) = parseXY point
      distance = manhattanDistance (x, y) beacon
   in ((x, y, distance), parseBeacon rest)

parseCave :: String -> Cave
parseCave input =
  let sensorsAndBeacons = map parseSensor $ lines input
      bs = nub $ map snd sensorsAndBeacons
      ss = map fst sensorsAndBeacons
   in Cave {sensors = ss, beacons = bs}

manhattanDistance :: Point -> Point -> Int
manhattanDistance (a1, b1) (a2, b2) = abs (a1 - a2) + abs (b1 - b2)

isBetween :: (Ord a) => a -> (a, a) -> Bool
isBetween x (a, b) = x >= a && x <= b

intersectsRow :: Int -> Sensor -> Bool
intersectsRow y s = y `isBetween` sensorBoundsY s
  where
    sensorBoundsY (_, y', r) = (y' - r, y' + r)

spanRow :: Int -> Sensor -> Point
spanRow y (xs, ys, rs) =
  let dx = rs - abs (y - ys)
   in (xs - dx, xs + dx)

flattenSpan :: [Point] -> [Int]
flattenSpan [] = []
flattenSpan ((a1, a2):xs) = [a1 .. a2] ++ flattenSpan xs

reduceSpan :: [Point] -> [Point]
reduceSpan [] = []
reduceSpan [x] = [x]
reduceSpan ((a1, b1):(a2, b2):xs)
  | a1 <= a2 && b1 >= b2 = reduceSpan ((a1, b1) : xs)
  | a1 >= a2 && b1 <= b2 = reduceSpan ((a2, b2) : xs)
  | a1 <= b2 && b1 >= b2 = reduceSpan ((a2, b1) : xs)
  | a1 <= a2 && b1 >= a2 = reduceSpan ((a1, b2) : xs)
  | otherwise = (a1, b1) : reduceSpan ((a2, b2) : xs)

subtractSpans :: Point -> [Point] -> [Point]
subtractSpans a [] = [a]
subtractSpans (a1, b1) ((a2, b2):xs)
  | a1 <= a2 && b1 >= b2 =
    subtractSpans (a1, a2 - 1) xs ++ subtractSpans (b2 + 1, b1) xs
  | a1 >= a2 && b1 <= b2 = []
  | a1 <= b2 && b1 >= b2 = subtractSpans (b2 + 1, b1) xs
  | a1 <= a2 && b1 >= a2 = subtractSpans (a1, a2 - 1) xs
  | otherwise = subtractSpans (a1, b1) xs

sensorsCoveringRow :: Int -> Cave -> [Sensor]
sensorsCoveringRow y = filter (intersectsRow y) . sensors

coveredPositionsAtRow :: Int -> Cave -> [Point]
coveredPositionsAtRow y cave =
  let ss = sensorsCoveringRow y cave
      ranges = sort $ map (spanRow y) ss
      flattened = map (, y) . flattenSpan . reduceSpan $ ranges
   in filter (`notElem` beacons cave) flattened

uncoveredPositionsAtRow :: Int -> Int -> Cave -> [Point]
uncoveredPositionsAtRow maxX y cave =
  let ss = sensorsCoveringRow y cave
      ranges = sort $ map (spanRow y) ss
      uncoveredRows = flattenSpan $ subtractSpans (0, maxX) ranges
   in map (, y) uncoveredRows

findDistressBeacon :: Int -> Cave -> Point
findDistressBeacon maxY c =
  case filter (not . null) [uncoveredPositionsAtRow maxY y c | y <- [0 .. maxY]] of
    [[a]] -> a

tuningFrequency :: Point -> Int
tuningFrequency (x, y) = x * factor + y
  where
    factor = 4000000

partOne :: String -> Int
partOne = length . coveredPositionsAtRow y . parseCave
  where
    y = 2000000

partTwo :: String -> Int
partTwo = tuningFrequency . findDistressBeacon y . parseCave
  where
    y = 4000000
