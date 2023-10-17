{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.List
import Data.List.Split

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Monkey = Monkey
    { monkeyId :: Int
    , items :: [Int]
    , op :: Int -> Int
    , test :: Int
    , ifTrue :: Int
    , ifFalse :: Int
    }

instance Show Monkey where
    show (Monkey id items op test ifTrue ifFalse) =
        "Monkey { id = " ++
        show id ++
        ", items = " ++
        show items ++
        ", test = " ++
        show test ++
        ", ifTrue = " ++ show ifTrue ++ ", ifFalse = " ++ show ifFalse ++ "}"

splitInput :: String -> [[String]]
splitInput = splitWhen (== "") . lines

solve = map parseMonkey . splitInput

parseId :: String -> Int
parseId (stripPrefix "Monkey " -> Just id) =
    case id of
        [id, ':'] -> read [id]

parseStartingItems :: String -> [Int]
parseStartingItems (stripPrefix "  Starting items: " -> Just str) =
    let items = takeWhile (/= '\n') str
        nums = '[' : filter (not . flip elem ['\n', ' ']) items ++ "]"
     in read nums

parseOperation :: String -> (Int -> Int)
parseOperation (stripPrefix "  Operation: new = old " -> Just str) =
    op $ words str
  where
    op :: [String] -> Int -> Int
    op ["*", "old"] = (^ 2)
    op ["*", num] = (*) $ read num
    op ["+", "old"] = (* 2)
    op ["+", num] = (+) $ read num

parseTest :: String -> Int
parseTest (stripPrefix "  Test: divisible by " -> Just num) = read num

parseIfTrue :: String -> Int
parseIfTrue (stripPrefix "    If true: throw to monkey " -> Just num) = read num

parseIfFalse :: String -> Int
parseIfFalse (stripPrefix "    If false: throw to monkey " -> Just num) =
    read num

parseMonkey :: [String] -> Monkey
parseMonkey [idStr, itemsStr, opStr, testStr, ifTrue, ifFalse] =
    Monkey
        { monkeyId = parseId idStr
        , items = parseStartingItems itemsStr
        , op = parseOperation opStr
        , test = parseTest testStr
        , ifTrue = parseIfTrue ifTrue
        , ifFalse = parseIfFalse ifFalse
        }
