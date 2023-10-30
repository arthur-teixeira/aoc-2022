module Day04 where

main :: IO ()
main = interact $ show . solve

solve = numContainedSections . map (overlapsSection . parseLine) . lines

data Section =
  Section Int Int
  deriving (Show)

data Pair =
  Pair Section Section
  deriving (Show)

chopWhen :: (a -> Bool) -> [a] -> ([a], [a])
chopWhen f xs = case break f xs of
  (ys, z:zs) -> (ys, zs)

parseLine :: String -> Pair
parseLine input =
  case chopWhen (== ',') input of
    (xs, ys) -> Pair (parseSection xs) (parseSection ys)


parseSection :: String -> Section
parseSection input =
  case chopWhen (== '-') input of
    (xs, ys) -> Section (read xs) (read ys)

overlapsSection :: Pair -> Bool
overlapsSection (Pair a b) = a `overlapsWith` b || b `overlapsWith` a
  where
    overlapsWith (Section a1 b1) (Section a2 b2) = b1 >= a2 && a1 <= b2

numContainedSections :: [Bool] -> Int
numContainedSections = sum . map fromEnum
