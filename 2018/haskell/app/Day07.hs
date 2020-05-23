module Day07 where

import Day07.Day07 as Day07

main :: IO ()
main = do

  input <- readFile "src/Day07/input.txt"

  -- ABGKCMVWYDEHFOPQUILSTNZRJX
  putStrLn "Solving Day07A..."
  print $
    Day07.solveA input

  -- 898
  putStrLn "Solving Day07B..."
  print $
    Day07.solveB input
