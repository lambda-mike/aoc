module Day01 where

import Day01.Day01 as Day01

main :: IO ()
main = do

  input <- readFile "src/Day01/input.txt"

  -- 510
  putStrLn "Solving Day01A..."
  print $
    Day01.solveA input

  -- 69074
  putStrLn "Solving Day01B..."
  print $
    Day01.solveB input
