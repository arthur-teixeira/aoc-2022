module Main
  ( main
  ) where

import qualified Data.Map as M
import Day (DaySolver)
import qualified Day01 as D01
import qualified Day02 as D02
import qualified Day03 as D03
import qualified Day04 as D04
import qualified Day05 as D05
import qualified Day06 as D06
import qualified Day07 as D07
import qualified Day08 as D08
import qualified Day09 as D09
import qualified Day10 as D10
import qualified Day11 as D11
import qualified Day12 as D12
import qualified Day13 as D13
import qualified Day14 as D14
import qualified Day15 as D15
import System.Environment (getArgs)

days :: M.Map Int (DaySolver, String)
days =
  M.fromList
    $ zip
        [1 ..]
        [ (D01.solveDay, "01.txt")
        , (D02.solveDay, "02.txt")
        , (D03.solveDay, "03.txt")
        , (D04.solveDay, "04.txt")
        , (D05.solveDay, "05.txt")
        , (D06.solveDay, "06.txt")
        , (D07.solveDay, "07.txt")
        , (D08.solveDay, "08.txt")
        , (D09.solveDay, "09.txt")
        , (D10.solveDay, "10.txt")
        , (D11.solveDay, "11.txt")
        , (D12.solveDay, "12.txt")
        , (D13.solveDay, "13.txt")
        , (D14.solveDay, "14.txt")
        , (D15.solveDay, "15.txt")
        ]

main :: IO ()
main = do
  [day] <- getArgs
  case M.lookup (read day) days of
    Just (solver, inputFile) -> do
      input <- readFile $ "inputs/" <> inputFile
      putStrLn $ "Day " <> day
      solver input
    Nothing -> putStrLn "Invalid day"
