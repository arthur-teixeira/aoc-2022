module Main where

import Data.Char
import Data.List.Split (splitWhen)

main :: IO ()
main = do
   contents <- getContents
   putStrLn $ "Part One: " <> show (partOne contents)

data Packet
    = Number Int
    | List [Packet]
    deriving (Show, Eq, Read)

instance Ord Packet where
    (Number a) `compare` (Number b) = a `compare` b
    a@(Number _) `compare` b@(List _) = List [a] `compare` b
    a@(List _) `compare` b@(Number _) = a `compare` List [b]
    (List a) `compare` (List b) =
        foldr (<>) (length a `compare` length b) (zipWith compare a b)

type Pair = (Packet, Packet)

type Indexed a = (Int, a)

partOne = sum . map doPair . index . parseInput

parseInput :: String -> [Pair]
parseInput = map parsePair . splitWhen (== "") . lines

parsePair :: [String] -> Pair
parsePair [a,b] = (parseLine a, parseLine b)

parseLine :: String -> Packet
parseLine "" = List []
parseLine xs = List [read $ process xs]
  where
    process "" = ""
    process ('[':ys) = "List [" ++ process ys
    process (' ':ys) = ' ' : process ys
    process (',':ys) = ',' : process ys
    process (']':ys) = ']' : process ys
    process xs =
        "Number " ++ takeWhile isNum xs ++ (process . dropWhile isNum) xs
    isNum = flip elem "-0123456789"

index :: [a] -> [(Int, a)]
index = zip [1..]

doPair :: Indexed Pair -> Int
doPair (idx, (a, b)) 
    | a < b = idx
    | otherwise = 0
