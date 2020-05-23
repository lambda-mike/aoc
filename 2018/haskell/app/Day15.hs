module Day15 where

import Day15.Day15 as Day15

main :: IO ()
main = do
  input <-
    readFile "src/Day15/input.txt"
  putStrLn "Solving Day15A..."
  -- 181522
  print $
    Day15.solve input
  putStrLn "Solving Day15B..."
  -- 68324
  print $
    Day15.solveB input

