{-# LANGUAGE ViewPatterns, TupleSections #-}

module Day15
  ( solveDay
  ) where

import Data.List (nub, sort, stripPrefix)
import Data.List.Split (splitWhen)
import Data.Maybe (mapMaybe)
import Day (DaySolver)

solveDay :: DaySolver
solveDay input = do
  putStrLn $ "Part One: " <> show (partOne input)

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
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

isBetween :: (Ord a) => a -> (a, a) -> Bool
isBetween x (a, b) = x >= a && x <= b

intersectsRow :: Int -> Sensor -> Bool
intersectsRow y s = y `isBetween` sensorBoundsY s
  where
    sensorBoundsY (_, y', r) = (y' - r, y' + r)

spanRow :: Int -> Sensor -> Maybe (Int, Int)
spanRow y (xs, ys, rs)
  | dx > 0 = Just (xs - dx, xs + dx)
  | otherwise = Nothing
  where
    dx = rs - abs (y - ys)

flattenSpan :: [(Int, Int)] -> [Int]
flattenSpan [] = []
flattenSpan ((xm, xM):xs) = [xm .. xM] ++ flattenSpan xs

reduceSpan :: [(Int, Int)] -> [(Int, Int)]
reduceSpan [] = []
reduceSpan [x] = [x]
reduceSpan ((a1, b1):(a2, b2):xs)
  | a1 <= a2 && b1 >= b2 = reduceSpan ((a1, b1) : xs)
  | a1 >= a2 && b1 <= b2 = reduceSpan ((a2, b2) : xs)
  | a1 <= b2 && b1 >= b2 = reduceSpan ((a2, b1) : xs)
  | a1 <= a2 && b1 >= a2 = reduceSpan ((a1, b2) : xs)
  | otherwise = (a1, b1) : reduceSpan ((a2, b2) : xs)

sensorsCoveringRow :: Int -> Cave -> [Sensor]
sensorsCoveringRow y = filter (intersectsRow y) . sensors

positionsCoveredAtRow :: Int -> Cave -> [(Int, Int)]
positionsCoveredAtRow y cave =
  let ss = sensorsCoveringRow y cave
      ranges = sort $ mapMaybe (spanRow y) ss
      flattened = map (, y) (flattenSpan . reduceSpan $ ranges)
   in filter (`notElem` beacons cave) flattened

partOne :: String -> Int
partOne = length . positionsCoveredAtRow y . parseCave
  where
    y = 2000000
