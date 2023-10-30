{-# LANGUAGE ViewPatterns #-}

module Day11 where

import Data.List (stripPrefix, sortBy)
import Data.List.Split
import qualified Data.Map as M
import Data.Ord

main :: IO ()
main = do
  contents <- getContents
  putStrLn $ partOne contents
  putStrLn $ partTwo contents

data Monkey = Monkey
  { monkeyId :: Int
  , items :: [Int]
  , op :: Int -> Int
  , test :: Int
  , ifTrue :: Int
  , ifFalse :: Int
  , nInspected :: Int
  }

instance Show Monkey where
  show (Monkey mId items _ test ifTrue ifFalse inspected) =
    "Monkey { id = "
      ++ show mId
      ++ ", items = "
      ++ show items
      ++ ", test = "
      ++ show test
      ++ ", ifTrue = "
      ++ show ifTrue
      ++ ", ifFalse = "
      ++ show ifFalse
      ++ ", nInspected = "
      ++ show inspected
      ++ "}"

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes x f = foldr1 (.) (replicate x f)

splitInput :: String -> [[String]]
splitInput = splitWhen (== "") . lines

partOne :: String -> String
partOne xs =
  "Part one: " ++ show (getMonkeyBusiness . doRoundsPartOne . parse $ xs)

partTwo :: String -> String
partTwo xs =
  "Part two: " ++ show (getMonkeyBusiness . doRoundsPartTwo . parse $ xs)

parse :: String -> MonkeyMap
parse = makeMonkeyMap . map parseMonkey . splitInput

parseId :: String -> Int
parseId (stripPrefix "Monkey " -> Just mId) =
    read [head mId]

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
    op ["*", "old"] = (^ (2 :: Int))
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
    , nInspected = 0
    }

makeMonkeyMap :: [Monkey] -> MonkeyMap
makeMonkeyMap = foldr (\m -> M.insert (monkeyId m) m) M.empty

setMonkeyItems :: [Int] -> Monkey -> Monkey
setMonkeyItems items m =
  Monkey
    { monkeyId = monkeyId m
    , items = items
    , op = op m
    , test = test m
    , ifTrue = ifTrue m
    , ifFalse = ifFalse m
    , nInspected = nInspected m
    }

increaseMonkeyCounter :: Monkey -> Monkey
increaseMonkeyCounter m =
  Monkey
    { monkeyId = monkeyId m
    , items = items m
    , op = op m
    , test = test m
    , ifTrue = ifTrue m
    , ifFalse = ifFalse m
    , nInspected = nInspected m + 1
    }

type MonkeyMap = M.Map Int Monkey

type WorryLevelModifier = Int -> Int

partOneModifier :: WorryLevelModifier
partOneModifier = flip div 3

-- We can use https://en.wikipedia.org/wiki/Chinese_remainder_theorem
-- because all of the monkey test values are coprime to each other
getPartTwoModifier :: MonkeyMap -> WorryLevelModifier
getPartTwoModifier ms =
  let monkeys = map snd $ M.toList ms
      products = product $ map test monkeys
   in (`mod` products)

doMonkeyPartOne :: MonkeyMap -> Int -> MonkeyMap
doMonkeyPartOne = doMonkey partOneModifier

doMonkeyPartTwo :: MonkeyMap -> Int -> MonkeyMap
doMonkeyPartTwo m = doMonkey (getPartTwoModifier m) m

doMonkey :: WorryLevelModifier -> MonkeyMap -> Int -> MonkeyMap
doMonkey modifier m mId
  | let Just monkey = M.lookup mId m
     in null (items monkey) = m
  | otherwise =
    let Just monkey = M.lookup mId m
        itemList = items monkey
        (item:rest) = itemList
        newItemValue = modifier $ op monkey item
        targetId =
          (if newItemValue `mod` test monkey == 0
             then ifTrue
             else ifFalse)
            monkey
        Just target = M.lookup targetId m
        targetItems = items target ++ [newItemValue]
        updatedTarget = setMonkeyItems targetItems target
        updatedMonkey = increaseMonkeyCounter . setMonkeyItems rest $ monkey
        updatedMap =
          M.insert targetId updatedTarget . M.insert mId updatedMonkey $ m
     in doMonkey modifier updatedMap mId

doRoundsPartOne :: MonkeyMap -> MonkeyMap
doRoundsPartOne = applyNTimes 20 doRoundPartOne

doRoundsPartTwo :: MonkeyMap -> MonkeyMap
doRoundsPartTwo = applyNTimes 10000 doRoundPartTwo

doRoundPartTwo :: MonkeyMap -> MonkeyMap
doRoundPartTwo m =
  let ids = M.keys m
   in foldl doMonkeyPartTwo m ids

doRoundPartOne :: MonkeyMap -> MonkeyMap
doRoundPartOne m =
  let ids = M.keys m
   in foldl doMonkeyPartOne m ids

getInspections :: MonkeyMap -> [Int]
getInspections = map (nInspected . snd) . M.toList

getMonkeyBusiness :: MonkeyMap -> Int
getMonkeyBusiness = product . take 2 . sortBy (comparing Data.Ord.Down) . getInspections
