module Day03 where

import Day03.Day03 as Day03

main :: IO ()
main = do

  input <- readFile "src/Day03/input.txt"

  -- 101781
  putStrLn "Solving Day03A..."
  print $
    Day03.solveA input

  -- 909
  putStrLn "Solving Day03B..."
  print $
    Day03.solveB input
