module Main where

main :: IO ()
main = interact $ show . solve

solve :: String -> Int
solve xs = sum $ roundResult <$> parseInput xs

data Shape
    = Rock
    | Paper
    | Scissors
    deriving (Eq, Show)

data Outcome
    = Win
    | Loss
    | Draw
    deriving (Eq, Show)

data Round =
    Round Shape Outcome
    deriving (Show)

parsePlay :: Char -> Shape
parsePlay inp
    | inp == 'A' = Rock
    | inp == 'B' = Paper
    | inp == 'C' = Scissors

parseOutcome :: Char -> Outcome
parseOutcome inp
    | inp == 'X' = Loss
    | inp == 'Y' = Draw
    | inp == 'Z' = Win

parseRound :: String -> Round
parseRound (opp:_:me:_) = Round (parsePlay opp) (parseOutcome me)

parseInput :: String -> [Round]
parseInput inp = map parseRound $ lines inp

shapePoints :: Shape -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

win = 6

draw = 3

loss = 0

roundResult :: Round -> Int
roundResult (Round a Draw) = draw + shapePoints a
roundResult (Round Rock Win) = shapePoints Paper + win
roundResult (Round Rock Loss) = shapePoints Scissors + loss
roundResult (Round Paper Win) = shapePoints Scissors + win
roundResult (Round Paper Loss) = shapePoints Rock + loss
roundResult (Round Scissors Win) = shapePoints Rock + win
roundResult (Round Scissors Loss) = shapePoints Paper + loss
