module Day02 where

import Day02.Day02 as Day02

main :: IO ()
main = do

  input <- readFile "src/Day02/input.txt"

  -- 7221
  putStrLn "Solving Day02A..."
  print $
    Day02.solveA input

  -- mkcdflathzwsvjxrevymbdpoq
  putStrLn "Solving Day02B..."
  print $
    Day02.solveB input
