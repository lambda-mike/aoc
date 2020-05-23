module Day05 where

import Day05.Day05 as Day05

main :: IO ()
main = do

  input <- readFile "src/Day05/input.txt"

  -- 9078
  putStrLn "Solving Day05A..."
  print $
    Day05.solveA input

  -- 5698
  putStrLn "Solving Day05B..."
  print $
    Day05.solveB input
