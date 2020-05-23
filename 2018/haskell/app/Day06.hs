module Day06 where

import Day06.Day06 as Day06

main :: IO ()
main = do

  input <- readFile "src/Day06/input.txt"

  -- 4342
  putStrLn "Solving Day06A..."
  print $
    Day06.solveA input

  -- 42966
  putStrLn "Solving Day06B..."
  print $
    Day06.solveB Day06.threshold input
