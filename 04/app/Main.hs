module Main where

main :: IO ()
main = interact $ show . solve

solve = numContainedSections . map (containsSection . parseLine) . lines

data Section =
  Section Int Int
  deriving (Show)

data Pair =
  Pair Section Section
  deriving (Show)

parseLine :: String -> Pair
parseLine input =
  case span (/= ',') input of
    (xs, ys) -> Pair (parseSection xs) (parseSection $ drop 1 ys)

parseSection :: String -> Section
parseSection input =
  case span (/= '-') input of
    (xs, ys) -> Section (read xs) (read $ drop 1 ys)

containsSection :: Pair -> Bool
containsSection (Pair a b) = a `contains` b || b `contains` a
  where
    contains (Section a1 b1) (Section a2 b2) = a1 <= a2 && b1 >= b2

numContainedSections :: [Bool] -> Int
numContainedSections = sum . map (\y -> if y then 1 else 0)
