module Day06 where
import Data.List (tails)

main :: IO ()
main = interact $ show . firstUnique . substrs

substrs :: String -> [String]
substrs xs = filter ((== 14) . length) $ map (take 14) $ tails xs

firstUnique :: [String] -> Int
firstUnique xs = firstUnique' xs $ (length . head) xs

firstUnique' :: [String] -> Int -> Int
firstUnique' [] _ = 0
firstUnique' (x:xs) n 
    | areUnique x = n
    | otherwise = firstUnique' xs (n + 1)

areUnique :: String -> Bool
areUnique [] = True
areUnique (x:xs)
    | x `elem` xs = False
    | otherwise = areUnique xs
