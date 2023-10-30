module Day03 where

import Data.List
import Data.Maybe

main :: IO ()
main = interact $ (++ "\n") . show . solve

chunksOf :: Int -> [a] -> [[a]]
chunksOf size xs
  | length xs > size =
    case splitAt size xs of
      (chunk, ys) -> chunk : chunksOf size ys
  | otherwise = [xs]

solve = sum . map itemPriority . chunksOf 3 . lines
  where
    itemPriority = getItemPriority . getCommonItem . tuplify3

tuplify3 :: [a] -> (a, a, a)
tuplify3 (x:y:z:_) = (x, y, z)

getCommonItem :: (String, String, String) -> Char
getCommonItem (x:xs, y, z)
  | x `elem` y && x `elem` z = x
  | otherwise = getCommonItem (xs, y, z)

getItemPriority :: Char -> Int
getItemPriority x = (1 +) $ fromMaybe 0 (x `elemIndex` chars)
  where
    chars = ['a' .. 'z'] ++ ['A' .. 'Z']
