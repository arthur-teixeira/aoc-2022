{-# LANGUAGE ViewPatterns #-}

module Day16 where

import Control.Monad.State
import Data.Char (isAlpha, isNumber)
import Data.List (nub, stripPrefix)
import Data.List.Split (splitWhen)
import qualified Data.Map as M
import Day (DaySolver)
import Debug.Trace (trace)

solveDay :: DaySolver
solveDay = print . parseValves

type Graph = M.Map String [String]

data Valve = Valve
    { valveId :: String
    , flowRate :: Int
    } deriving (Show)

parseName :: String -> (String, String)
parseName (stripPrefix "Valve " -> Just xs) = (take 2 xs, drop 3 xs)

parseFlowRate :: String -> (Int, String)
parseFlowRate (stripPrefix "has flow rate=" -> Just xs) =
    (read $ takeWhile isNumber xs, dropWhile (not . isAlpha) xs)

parseAdjacent :: String -> [String]
parseAdjacent (stripPrefix "tunnel leads to valve " -> Just xs) = [xs]

parseAdjacents :: String -> [String]
parseAdjacents input =
    case stripPrefix "tunnels lead to valves " input of
        Just xs -> splitWhen (== ',') . filter (/= ' ') $ xs
        Nothing -> parseAdjacent input

parseValve :: String -> (Graph, [Valve]) -> (Graph, [Valve])
parseValve xs (graph, valves) =
    let (vId, rest1) = parseName xs
        (rate, rest2) = parseFlowRate rest1
        adjacents = parseAdjacents rest2
     in (M.insert vId adjacents graph, Valve vId rate : valves)

parseValves :: String -> (Graph, [Valve])
parseValves xs = foldr parseValve (M.empty, []) $ lines xs

unsafeLookup :: (Ord a) => M.Map a b -> a -> b
unsafeLookup m key =
    case M.lookup key m of
        Just i -> i
        Nothing -> error "not found"

type Path = [String]

type Queue = [(String, [String])]

type Visited = [String]

type BFSState = State (Queue, Visited) Path

bfs :: String -> String -> Graph -> Path
bfs from to graph = evalState (bfs' to graph) ([(from, [from])], [])

bfs' :: String -> Graph -> BFSState
bfs' to graph = do
    (queue, visited) <- get
    let ((vertex, path):queueRest) = queue
        ns = neighbors vertex
        toVisit = filter (`notElem` visited) ns
        next = map (\n -> (n, path ++ [n])) toVisit
     in if to `elem` ns
            then return $ path ++ [to]
            else put (queueRest ++ next, visited ++ toVisit) >> bfs' to graph
  where
    neighbors = unsafeLookup graph
