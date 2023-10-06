module Main where

main :: IO ()
main = interact solve

solve :: String -> String
solve xs = show . sum $ roundPoints <$> parseInput xs

data Play
    = Rock
    | Paper
    | Scissors
    deriving (Eq, Show)

data Round =
    Round Play Play
    deriving (Show)

parsePlay :: Char -> Play
parsePlay inp
    | inp == 'A' || inp == 'X' = Rock
    | inp == 'B' || inp == 'Y' = Paper
    | inp == 'C' || inp == 'Z' = Scissors

parseRound :: String -> Round
parseRound (opp:_:me:_) = Round (parsePlay opp) (parsePlay me)

parseInput :: String -> [Round]
parseInput inp = map parseRound $ lines inp

shapePoints :: Play -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

win = 6
draw = 3
loss = 0

roundResult :: Round -> Int
roundResult (Round Rock Paper) = win
roundResult (Round Paper Scissors) = win
roundResult (Round Scissors Rock) = win
roundResult (Round Rock Scissors) = loss
roundResult (Round Paper Rock) = loss
roundResult (Round Scissors Paper) = loss
roundResult _ = draw

roundPoints :: Round -> Int
roundPoints r@(Round _ m) = shapePoints m + roundResult r

