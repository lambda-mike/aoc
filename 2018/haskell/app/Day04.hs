module Day04 where

import Day04.Day04 as Day04

main :: IO ()
main = do

  input <- readFile "src/Day04/input.txt"

  -- 12504
  putStrLn "Solving Day04A..."
  print $
    Day04.solveA input

  -- 139543
  putStrLn "Solving Day04B..."
  print $
    Day04.solveB input
