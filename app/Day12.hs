{-# LANGUAGE TupleSections #-}

module Day12 where

import Control.Monad.State
import qualified Data.Array.IArray as A
import Data.List (nub)
import Linear.V2

import Control.Applicative

main :: IO ()
main = do
    contents <- getContents
    putStrLn $ "Part One: " <> show (partOne contents)
    putStrLn $ "Part Two: " <> show (partTwo contents)

partOne = evalState (starts >>= bfs) . inputToGraph . lines
partTwo = evalState (startsA >>= bfs) . inputToGraph . lines

type Edge = (Int, Int)

type Graph = A.Array (V2 Int) Char

inputToGraph :: [String] -> Graph
inputToGraph xs =
  A.listArray (V2 1 1, V2 (length xs) (length . head $ xs)) $ concat xs

type BFS = State Graph

steps :: [V2 Int] -> BFS [V2 Int]
steps xs = do
  xs' <- gets $ filter (not . (`elem` xs)) . nub . (`concatMap` xs) . neighbors
  modify (A.// map (, '|') xs) >> return xs'

bfs :: [V2 Int] -> BFS Int
bfs xs = do
  g <- get
  if any ((== 'E') . (g A.!)) xs
    then return 0
    else (+ 1) <$> (steps xs >>= bfs)

dirs = [V2 0 1, V2 1 0, V2 0 (-1), V2 (-1) 0]

starts :: BFS [V2 Int]
starts = gets $ map fst . filter ((== 'S') . snd) . A.assocs

startsA :: BFS [V2 Int]
startsA = gets $ map fst . filter ((`elem` ['S', 'a']) . snd) . A.assocs

neighbors :: Graph -> V2 Int -> [V2 Int]
neighbors g p =
  filter (canStep (g A.! p) . (g A.!)) . filter (A.inRange $ A.bounds g)
    $ map (p +) dirs

canStep :: Char -> Char -> Bool
canStep 'S' b = canStep 'a' b
canStep a 'E' = canStep a 'z'
canStep a b = fromEnum b - fromEnum a <= 1
