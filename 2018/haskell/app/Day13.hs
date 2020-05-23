module Day13 where

import Day13.Day13 as Day13

main :: IO ()
main = do

  input <- readFile "src/Day13/input.txt"

  -- (129,50)
  putStrLn "Solving Day13A..."
  print $
    Day13.solveA input

  -- (69,73)
  putStrLn "Solving Day13B..."
  print $
    Day13.solveB input
