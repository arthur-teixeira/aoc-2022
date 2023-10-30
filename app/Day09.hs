module Day09
  ( solveDay
  ) where

import Prelude hiding (Left, Right)

import Data.Foldable (minimumBy)
import Data.Function (on)
import qualified Data.Set as S
import Day (DaySolver)

solveDay :: DaySolver
solveDay input = do
  putStrLn $ partOne input
  putStrLn $ partTwo input

solve :: State -> String -> Int
solve state = S.size . visited . foldl (flip move) state . parse

startStateOne :: State
startStateOne = State (replicate 2 (0, 0)) $ S.fromList [(0, 0)]

startStateTwo :: State
startStateTwo = State (replicate 10 (0, 0)) $ S.fromList [(0, 0)]

partOne :: String -> String
partOne xs = "Part one: " ++ (show . solve startStateOne) xs

partTwo :: String -> String
partTwo xs = "Part two: " ++ (show . solve startStateTwo) xs

data Move =
  Move Char Int
  deriving (Show)

type Position = (Int, Int)

data State = State
  { body :: [Position]
  , visited :: S.Set Position
  } deriving (Show)

parseLine :: String -> Move
parseLine xs =
  case words xs of
    [dir, moves] -> Move (head dir) (read moves)

parse :: String -> [Move]
parse = map parseLine . lines

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes x f = foldr1 (.) (replicate x f)

moveUp :: Position -> Position
moveUp (x, y) = (x, y + 1)

moveDown :: Position -> Position
moveDown (x, y) = (x, y - 1)

moveLeft :: Position -> Position
moveLeft (x, y) = (x - 1, y)

moveRight :: Position -> Position
moveRight (x, y) = (x + 1, y)

moveOnce :: Char -> State -> State
moveOnce direction (State (h:tails) visited) =
  let newHead = mover h
      newTail = moveTails (newHead : tails)
      newVisited = S.insert (last newTail) visited
   in State (newHead : newTail) newVisited
  where
    mover =
      case direction of
        'U' -> moveUp
        'D' -> moveDown
        'L' -> moveLeft
        'R' -> moveRight

move :: Move -> State -> State
move (Move direction n) = applyNTimes n (moveOnce direction)

neighborhoodOf :: Position -> S.Set Position
neighborhoodOf (x, y) =
  S.fromList [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1]]

closestTo :: S.Set Position -> Position -> Position
closestTo options target = minimumBy (compare `on` distance target) options
  where
    distance :: Position -> Position -> Float
    distance (x, y) (x', y') =
      (fromIntegral x - fromIntegral x') ^ (2 :: Int)
        + (fromIntegral y - fromIntegral y') ^ (2 :: Int)

moveTails :: [Position] -> [Position]
moveTails [_] = []
moveTails (x:y:ys) =
  let newPos = moveTail x y
   in newPos : moveTails (newPos : ys)

moveTail :: Position -> Position -> Position
moveTail h t
  | t `elem` neighbors = t
  | otherwise = valid_positions `closestTo` h
  where
    neighbors = neighborhoodOf h
    valid_positions = neighborhoodOf t `S.intersection` neighbors
