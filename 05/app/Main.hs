module Main where

import Data.Char (isAlpha)
import Data.List (inits, stripPrefix)
import Data.Maybe (fromMaybe)
import GHC.Base (getTag)

main :: IO ()
main = interact solve

type Crate = Char

type Stack = [Crate]

newtype Pile =
  Pile [Stack]
  deriving (Show)

-- This works because every pile will always have the same length.
instance Semigroup Pile where
  (Pile a) <> (Pile b) = Pile (zipWith (<>) b a)

solve :: String -> String
solve xs =
  let (pileString, instructionsString) = pileAndInstructions xs
      pile = makePile pileString
      instructions = map parseInstruction instructionsString
      finalPile = foldl (flip applyInstruction) pile instructions
  in getTopFromStacks finalPile

pileAndInstructions :: String -> ([String], [String])
pileAndInstructions = break (== "") . lines

isEmpty :: String -> Bool
isEmpty = not . any isAlpha

getCrate :: String -> Crate
getCrate xs
  | isEmpty xs = ' '
  | otherwise = head $ filter (`notElem` "[] ") xs

getCratesInLine :: String -> [Crate]
getCratesInLine "" = ""
getCratesInLine xs =
  case splitAt 4 xs of
    (crate, ys) -> getCrate crate : getCratesInLine ys

chunksOf :: Int -> [a] -> [[a]]
chunksOf size xs
  | length xs > size =
    case splitAt size xs of
      (chunk, ys) -> chunk : chunksOf size ys
  | otherwise = [xs]

trim :: String -> String
trim = takeWhile (/= ' ')

pileFromLine :: [Crate] -> Pile
pileFromLine xs = Pile (chunksOf 1 xs)

trimPile :: Pile -> Pile
trimPile (Pile xs) = Pile (map trim xs)

makePile :: [String] -> Pile
makePile xs =
  trimPile
    $ foldr1 (<>)
    $ map (pileFromLine . getCratesInLine)
    $ init xs

data Instruction =
  Instruction Int Int Int
  deriving (Show)

parse :: String -> String -> Maybe (Int, String)
parse prefix xs = do
  noPrefix <- stripPrefix prefix xs
  case break (== ' ') noPrefix of
    (amount, rest) -> Just (read amount, rest)

parseAmount :: String -> Maybe (Int, String)
parseAmount = parse "move "

parseFrom :: String -> Maybe (Int, String)
parseFrom = parse " from "

parseTo :: String -> Maybe (Int, String)
parseTo = parse " to "

parseInstruction' :: String -> Maybe Instruction
parseInstruction' xs = do
  (amount, rest1) <- parseAmount xs
  (from, rest2) <- parseFrom rest1
  (to, rest3) <- parseTo rest2
  Just (Instruction amount from to)

parseInstruction :: String -> Instruction
parseInstruction xs = fromMaybe (Instruction 0 0 0) (parseInstruction' xs)

takeLast :: Int -> [a] -> [a]
takeLast n xs = reverse $ take n $ reverse xs

dropLast :: Int -> [a] -> [a]
dropLast n xs = reverse $ drop n $ reverse xs

dropFromStack :: Int -> Int -> Pile -> Pile
dropFromStack numStack n (Pile p) =
  case splitAt numStack p of
    (x, y:ys) -> Pile (x ++ dropLast n y : ys)

pushToStack :: Int -> Stack -> Pile -> Pile
pushToStack numStack elems (Pile p) =
  case splitAt numStack p of
    (x, y:ys) -> Pile (x ++ (y ++ elems) : ys)

applyInstruction :: Instruction -> Pile -> Pile
applyInstruction (Instruction amount from to) pile@(Pile p) =
  let elems = reverse $ takeLast amount (p !! (from - 1))
   in pushToStack (to - 1) elems (dropFromStack (from - 1) amount pile)

getTopFromStacks :: Pile -> [Crate]
getTopFromStacks (Pile p) = map last p

