module Day08 where

import Day08.Day08 as Day08

main :: IO ()
main = do

  input <- readFile "src/Day08/input.txt"

  -- 45618
  putStrLn "Solving Day08A..."
  print $
    Day08.solveA input

  -- 22306
  putStrLn "Solving Day08B..."
  print $
    Day08.solveB input
